/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */
package com.snowplowanalytics.iglu.jsonschema

import scala.collection.mutable
import java.util.regex.Pattern
import java.util.{HashMap => JHashMap}
import io.circe.{Json, JsonNumber, JsonObject, JsonObjectUnsafe}

/** Compiled schema representation. Immutable, reusable, optimized for repeated validation.
  */
sealed trait Schema {

  /** Validate JSON data against this schema.
    *
    * @param json
    *   The JSON data to validate
    * @param path
    *   The current JSON path (for error reporting)
    * @param remainingDepth
    *   Maximum remaining depth for recursive validation (prevents stack overflow)
    * @return
    *   Vector of validation errors (empty if valid)
    */
  def validate(json: Json, path: JsonPath, remainingDepth: Int): Vector[ValidationError]

  /** Fast validity check with short-circuit evaluation. No error construction, no path allocation.
    *
    * @param json
    *   The JSON data to validate
    * @param remainingDepth
    *   Maximum remaining depth for recursive validation
    * @return
    *   true if valid, false otherwise
    */
  def isValid(json: Json, remainingDepth: Int): Boolean
}

object Schema {

  /** Schema that always passes (empty schema {}). No recursion needed — there are no constraints. */
  case object Empty extends Schema {
    def validate(json: Json, path: JsonPath, remainingDepth: Int): Vector[ValidationError] = Vector.empty
    def isValid(json: Json, remainingDepth: Int): Boolean = true
  }

  /** Schema that always fails (boolean false schema) */
  case object Never extends Schema {
    def validate(json: Json, path: JsonPath, remainingDepth: Int): Vector[ValidationError] =
      Vector(ValidationError(path, "false", "schema is false, nothing can validate"))
    def isValid(json: Json, remainingDepth: Int): Boolean = false
  }

  /** Schema that always fails with additionalItems error */
  case object AdditionalItemsForbidden extends Schema {
    def validate(json: Json, path: JsonPath, remainingDepth: Int): Vector[ValidationError] =
      Vector(ValidationError.additionalItems(path))
    def isValid(json: Json, remainingDepth: Int): Boolean = false
  }

  // Mutable during compilation only, effectively immutable after.
  case class Ref(private[jsonschema] var target: Schema) extends Schema {
    def validate(json: Json, path: JsonPath, remainingDepth: Int): Vector[ValidationError] =
      target.validate(json, path, remainingDepth)
    def isValid(json: Json, remainingDepth: Int): Boolean =
      target.isValid(json, remainingDepth)
  }

  // Meta-schema support only. No event/entity schemas use allOf.
  // Kept as a separate variant so Compiled.validate has zero allOf overhead.
  case class WithAllOf(base: Schema, allOf: Vector[(Schema, Json)]) extends Schema {
    def validate(json: Json, path: JsonPath, remainingDepth: Int): Vector[ValidationError] = {
      base.validate(json, path, remainingDepth) ++
        allOf.flatMap { case (s, _) => s.validate(json, path, remainingDepth - 1) }
    }
    def isValid(json: Json, remainingDepth: Int): Boolean =
      base.isValid(json, remainingDepth) && allOf.forall { case (s, _) => s.isValid(json, remainingDepth - 1) }
  }

  /** Pre-resolved format validator with cached error pattern (no Map.get per validation) */
  case class CompiledFormat(name: String, validator: formats.Format, errorPattern: String)

  /** Composite schema with all keyword validations. Uses HashMap for O(1) property lookup (instead of SortedMap's O(log n)).
    */
  case class Compiled(
      // Type validation
      types: Option[TypeConstraint],
      `enum`: Option[(Set[Json], Vector[Json])],
      // String-only enum fast-path: Some(set) iff every enum value is a string.
      // Avoids Json.hashCode (O(n) for JNumber) on the hot isValid path.
      enumStrings: Option[Set[String]],

      // String constraints
      minLength: Option[Int],
      maxLength: Option[Int],
      pattern: Option[CompiledPattern],
      format: Option[CompiledFormat],

      // Number constraints
      minimum: Option[BigDecimal],
      maximum: Option[BigDecimal],
      exclusiveMinimum: Boolean,
      exclusiveMaximum: Boolean,
      // Exact-double precomputed at compile time. Double.NaN when absent or not exactly
      // representable (e.g. very high-precision decimals). Used by isValidNumberRaw to
      // avoid n.toBigDecimal (BiggerDecimal.fromLong) on the hot path.
      minimumD: Double,
      maximumD: Double,
      multipleOf: Option[BigDecimal],

      // Array constraints
      items: Option[ItemsConstraint],
      additionalItems: Option[Schema],
      minItems: Option[Int],
      maxItems: Option[Int],
      uniqueItems: Boolean,

      // Object constraints — Java HashMap for O(1) lookup with no Option boxing
      properties: JHashMap[String, Schema],
      patternProperties: Array[(CompiledPattern, Schema)],
      additionalProperties: Option[AdditionalProperties],
      required: Array[String],
      minProperties: Option[Int],
      maxProperties: Option[Int],

      // Dependencies: if key is present, either listed properties must exist or schema must validate
      dependencies: Vector[(String, Either[Vector[String], Schema])],

      // Composition - stores both compiled schema and original JSON for error messages
      anyOf: Vector[(Schema, Json)],
      oneOf: Vector[(Schema, Json)],
      not: Option[(Schema, Json)]
  ) extends Schema {

    // Pre-computed flags: skip entire validation branches when no constraints exist
    private val hasStringConstraints: Boolean =
      minLength.isDefined || maxLength.isDefined || pattern.isDefined || format.isDefined
    private val hasLengthConstraints: Boolean =
      minLength.isDefined || maxLength.isDefined
    private val hasNumberConstraints: Boolean =
      minimum.isDefined || maximum.isDefined || multipleOf.isDefined
    private val hasPatternProperties: Boolean = patternProperties.nonEmpty
    private val hasRequired: Boolean = required.nonEmpty
    private val hasDependencies: Boolean = dependencies.nonEmpty
    private val hasComposition: Boolean = anyOf.nonEmpty || oneOf.nonEmpty || not.isDefined
    // Pre-computed sizes: avoid Vector.length virtual dispatch in while-loop bounds
    private val patternPropertiesSize: Int = patternProperties.length
    private val requiredSize: Int = required.length
    private val dependenciesSize: Int = dependencies.length
    private val anyOfSize: Int = anyOf.length
    private val oneOfSize: Int = oneOf.length
    // Pre-computed at schema compile time: enum error display list avoids per-call List allocation
    private val enumErrorStrings: List[String] = `enum`.fold(Nil: List[String]) { case (_, vec) =>
      vec.iterator.map(v => v.asString.getOrElse(v.noSpaces)).toList
    }

    def validate(json: Json, path: JsonPath, remainingDepth: Int): Vector[ValidationError] = {
      if (remainingDepth <= 0) {
        return Vector(ValidationError.maxDepthExceeded(path))
      }

      // Lazily-initialized buffer — zero VectorBuilder allocation when document is valid
      val errors = new LazyErrors

      // Type validation
      types match {
        case Some(tc) =>
          val actualType = JsonType.of(json)
          tc match {
            case TypeConstraint.Single(expected) =>
              if (!matchesType(actualType, expected))
                errors += ValidationError.typeMismatch(path, actualType.name, expected.name)

            case TypeConstraint.Union(expectedTypes, mask) =>
              if ((mask & actualType.matchMask) == 0)
                errors += ValidationError.typeMismatchUnion(path, actualType.name, expectedTypes.map(_.name))
          }
        case None => ()
      }

      // Enum validation — string fast-path avoids Json.hashCode
      `enum` match {
        case Some((allowedSet, _)) =>
          val inEnum = enumStrings match {
            case Some(ss) =>
              val s = JsonObjectUnsafe.stringValue(json) // null if not a JString
              s != null && ss.contains(s)
            case None => allowedSet.contains(json)
          }
          if (!inEnum)
            errors += ValidationError.`enum`(path, enumErrorStrings)
        case None => ()
      }

      // Type-specific validations — direct dispatch, zero closure allocation.
      val obj = JsonObjectUnsafe.objectValue(json)
      if (obj != null) {
        validateObject(obj, path, remainingDepth, errors)
      } else {
        val arr = JsonObjectUnsafe.arrayValue(json)
        if (arr != null) {
          validateArray(arr, path, remainingDepth, errors)
        } else {
          val s = JsonObjectUnsafe.stringValue(json)
          if (s != null) {
            if (hasStringConstraints) validateString(s, path, errors)
          } else {
            val n = JsonObjectUnsafe.numberValue(json)
            if (n != null && hasNumberConstraints) {
              validateNumberRaw(n, path, errors)
            }
          }
        }
      }

      // Composition validations
      if (hasComposition) validateComposition(json, path, remainingDepth, errors)

      errors.result()
    }

    def isValid(json: Json, remainingDepth: Int): Boolean = {
      if (remainingDepth <= 0) return false

      // Type validation
      types match {
        case Some(tc) =>
          val actualType = JsonType.of(json)
          tc match {
            case TypeConstraint.Single(expected) =>
              if (!matchesType(actualType, expected)) return false
            case TypeConstraint.Union(_, mask) =>
              if ((mask & actualType.matchMask) == 0) return false
          }
        case None => ()
      }

      // Enum validation — string fast-path avoids Json.hashCode
      `enum` match {
        case Some((allowedSet, _)) =>
          enumStrings match {
            case Some(ss) =>
              val s = JsonObjectUnsafe.stringValue(json) // null if not a JString
              if (s == null || !ss.contains(s)) return false
            case None =>
              if (!allowedSet.contains(json)) return false
          }
        case None => ()
      }

      // Type-specific validations — direct dispatch, zero closure allocation.
      // Replaces json.fold(...) which allocates 5-6 lambda closures per call and
      // boxes the Boolean return. Null-returning extractors avoid Option boxing too.
      val obj = JsonObjectUnsafe.objectValue(json)
      if (obj != null) {
        if (!isValidObject(obj, remainingDepth)) return false
      } else {
        val arr = JsonObjectUnsafe.arrayValue(json)
        if (arr != null) {
          if (!isValidArray(arr, remainingDepth)) return false
        } else {
          val s = JsonObjectUnsafe.stringValue(json)
          if (s != null) {
            if (hasStringConstraints && !isValidString(s)) return false
          } else {
            val n = JsonObjectUnsafe.numberValue(json)
            if (n != null) {
              if (hasNumberConstraints && !isValidNumberRaw(n)) return false
            }
            // null and boolean: no type-specific validation needed
          }
        }
      }

      // Composition validations
      if (hasComposition && !isValidComposition(json, remainingDepth)) return false

      true
    }

    private def matchesType(actual: JsonType, expected: JsonType): Boolean =
      (actual, expected) match {
        case (a, e) if a == e                    => true
        case (JsonType.Integer, JsonType.Number) => true // integer is a valid number
        case _                                   => false
      }

    // --- validate helpers: append errors to shared builder ---

    private def validateString(
        s: String,
        path: JsonPath,
        errors: LazyErrors
    ): Unit = {
      if (hasLengthConstraints) {
        val codePointCount = s.codePointCount(0, s.length)
        minLength.foreach { min =>
          if (codePointCount < min) errors += ValidationError.minLength(path, min)
        }
        maxLength.foreach { max =>
          if (codePointCount > max) errors += ValidationError.maxLength(path, max)
        }
      }

      pattern.foreach { p =>
        if (!p.matches(s)) errors += ValidationError.pattern(path, p.source)
      }

      format.foreach { cf =>
        if (!cf.validator.validate(s)) {
          errors += ValidationError.format(path, cf.name, cf.errorPattern)
        }
      }
    }

    // validate fast-path: mirrors isValidNumberRaw, avoids n.toBigDecimal (BiggerDecimal.fromLong)
    // for Long-valued JSON numbers. On error paths BigDecimal(v) is used for messages — acceptable
    // since errors are exceptional. Non-Long values (e.g. 1.5, huge integers) fall through to validateNumber.
    private def validateNumberRaw(n: JsonNumber, path: JsonPath, errors: LazyErrors): Unit =
      n.toLong match {
        case Some(v) =>
          val vd = v.toDouble
          // minimum — double fast-path when minimumD is an exact double, BigDecimal otherwise
          if (!minimumD.isNaN) {
            if (exclusiveMinimum && vd <= minimumD)
              errors += ValidationError.minimum(path, minimum.get)
            else if (!exclusiveMinimum && vd < minimumD)
              errors += ValidationError.minimum(path, minimum.get)
          } else
            minimum match {
              case Some(min) if exclusiveMinimum && BigDecimal(v) <= min =>
                errors += ValidationError.minimum(path, min)
              case Some(min) if !exclusiveMinimum && BigDecimal(v) < min =>
                errors += ValidationError.minimum(path, min)
              case _ => ()
            }
          // maximum
          if (!maximumD.isNaN) {
            if (exclusiveMaximum && vd >= maximumD)
              errors += ValidationError.maximum(path, maximum.get)
            else if (!exclusiveMaximum && vd > maximumD)
              errors += ValidationError.maximum(path, maximum.get)
          } else
            maximum match {
              case Some(max) if exclusiveMaximum && BigDecimal(v) >= max =>
                errors += ValidationError.maximum(path, max)
              case Some(max) if !exclusiveMaximum && BigDecimal(v) > max =>
                errors += ValidationError.maximum(path, max)
              case _ => ()
            }
          // multipleOf — zero divisor filtered at compile time; use match to avoid Function1 closure
          multipleOf match {
            case Some(divisor) =>
              val q = vd / divisor.toDouble
              if (q.isInfinite || q.isNaN || q != Math.floor(q))
                errors += ValidationError.multipleOf(path, divisor)
            case None => ()
          }
        case None =>
          // Non-Long value (e.g. 1.5, very large integer): fall back to BigDecimal path
          validateNumber(n.toBigDecimal.getOrElse(BigDecimal(n.toDouble)), path, errors)
      }

    private def validateNumber(
        n: BigDecimal,
        path: JsonPath,
        errors: LazyErrors
    ): Unit = {
      minimum.foreach { min =>
        if (exclusiveMinimum) {
          if (n <= min) errors += ValidationError.minimum(path, min)
        } else {
          if (n < min) errors += ValidationError.minimum(path, min)
        }
      }
      maximum.foreach { max =>
        if (exclusiveMaximum) {
          if (n >= max) errors += ValidationError.maximum(path, max)
        } else {
          if (n > max) errors += ValidationError.maximum(path, max)
        }
      }
      multipleOf match {
        case Some(divisor) =>
          val quotient = n.toDouble / divisor.toDouble
          if (quotient.isInfinite || quotient.isNaN || quotient != Math.floor(quotient))
            errors += ValidationError.multipleOf(path, divisor)
        case None => ()
      }
    }

    private def validateArray(
        arr: Vector[Json],
        path: JsonPath,
        remainingDepth: Int,
        errors: LazyErrors
    ): Unit = {
      minItems match {
        case Some(min) if arr.length < min => errors += ValidationError.minItems(path, min)
        case _                             => ()
      }

      maxItems match {
        case Some(max) if arr.length > max => errors += ValidationError.maxItems(path, max)
        case _                             => ()
      }

      if (uniqueItems && arr.length > 1) {
        val seen = new mutable.HashSet[Json]()
        var idx = 0
        while (idx < arr.length) {
          if (!seen.add(arr(idx))) {
            errors += ValidationError.uniqueItems(path)
          }
          idx += 1
        }
      }

      items match {
        case Some(ItemsConstraint.Single(schema)) =>
          var idx = 0
          while (idx < arr.length) {
            errors ++= schema.validate(arr(idx), path.index(idx), remainingDepth - 1)
            idx += 1
          }
        case Some(ItemsConstraint.Tuple(schemas)) =>
          val len = math.min(arr.length, schemas.length)
          var idx = 0
          while (idx < len) {
            errors ++= schemas(idx).validate(arr(idx), path.index(idx), remainingDepth - 1)
            idx += 1
          }
          // additionalItems: validate items beyond tuple length
          additionalItems.foreach { addSchema =>
            var extraIdx = schemas.length
            while (extraIdx < arr.length) {
              errors ++= addSchema.validate(arr(extraIdx), path.index(extraIdx), remainingDepth - 1)
              extraIdx += 1
            }
          }
        case None =>
          // When items is absent, additionalItems has no effect (per spec)
          ()
      }
    }

    private def validateObject(
        obj: JsonObject,
        path: JsonPath,
        remainingDepth: Int,
        errors: LazyErrors
    ): Unit = {
      // Validate each property: properties → patternProperties → additionalProperties
      val keyIter = obj.keys.iterator
      while (keyIter.hasNext) {
        val key = keyIter.next()
        val value = JsonObjectUnsafe.getValue(obj, key)

        val propSchema = properties.get(key) // null if not found — no Option boxing
        val inProperties = propSchema != null
        if (inProperties) {
          errors ++= propSchema.validate(value, path.field(key), remainingDepth - 1)
        }

        // patternProperties: a key can match both properties and patternProperties
        var matchedPattern = false
        if (hasPatternProperties) {
          var ppIdx = 0
          while (ppIdx < patternPropertiesSize) {
            val (cp, schema) = patternProperties(ppIdx)
            if (cp.matches(key)) {
              matchedPattern = true
              errors ++= schema.validate(value, path.field(key), remainingDepth - 1)
            }
            ppIdx += 1
          }
        }

        // A key is "additional" only if NOT in properties AND matches no patternProperties pattern
        if (!inProperties && !matchedPattern) {
          additionalProperties match {
            case Some(AdditionalProperties.Forbidden) =>
              errors += ValidationError.additionalProperty(path, key)
            case Some(AdditionalProperties.Allowed(schema)) =>
              errors ++= schema.validate(value, path.field(key), remainingDepth - 1)
            case None =>
              () // Additional properties allowed by default
          }
        }
      }

      // Required properties
      if (hasRequired) {
        var reqIdx = 0
        while (reqIdx < requiredSize) {
          if (!obj.contains(required(reqIdx)))
            errors += ValidationError.required(path, required(reqIdx))
          reqIdx += 1
        }
      }

      // Dependencies
      if (hasDependencies) {
        var depIdx = 0
        while (depIdx < dependenciesSize) {
          val (prop, dep) = dependencies(depIdx)
          if (obj.contains(prop)) {
            dep match {
              case Left(requiredProps) =>
                var rpIdx = 0
                var anyMissing = false
                while (rpIdx < requiredProps.length) {
                  if (!obj.contains(requiredProps(rpIdx))) anyMissing = true
                  rpIdx += 1
                }
                if (anyMissing) errors += ValidationError.dependency(path, prop, requiredProps)
              case Right(schema) =>
                errors ++= schema.validate(Json.fromJsonObject(obj), path, remainingDepth - 1)
            }
          }
          depIdx += 1
        }
      }

      // Property count constraints
      minProperties match {
        case Some(min) if obj.size < min => errors += ValidationError.minProperties(path, min)
        case _                           => ()
      }

      maxProperties match {
        case Some(max) if obj.size > max => errors += ValidationError.maxProperties(path, max)
        case _                           => ()
      }
    }

    private def validateComposition(
        json: Json,
        path: JsonPath,
        remainingDepth: Int,
        errors: LazyErrors
    ): Unit = {
      // anyOf: at least one must match — fast isValid first, expensive validate only on failure
      if (anyOfSize > 0) {
        var anyMatches = false
        var i = 0
        while (i < anyOfSize && !anyMatches) {
          if (anyOf(i)._1.isValid(json, remainingDepth - 1)) anyMatches = true
          i += 1
        }
        if (!anyMatches) {
          // No branch matched — collect all branch errors for networknt-compatible reporting
          var j = 0
          while (j < anyOfSize) {
            errors ++= anyOf(j)._1.validate(json, path, remainingDepth - 1)
            j += 1
          }
        }
      }

      // oneOf: exactly one must match — count first, collect schemas only on error
      if (oneOfSize > 0) {
        var matchCount = 0
        var i = 0
        while (i < oneOfSize) {
          if (oneOf(i)._1.isValid(json, remainingDepth - 1)) matchCount += 1
          i += 1
        }
        if (matchCount == 0) {
          errors += ValidationError.oneOfNone(path, oneOf.length)
        } else if (matchCount > 1) {
          // Only build the matched schemas vector for the error message
          val matched = Vector.newBuilder[Json]
          var j = 0
          while (j < oneOfSize) {
            val (schema, json2) = oneOf(j)
            if (schema.isValid(json, remainingDepth - 1)) matched += json2
            j += 1
          }
          errors += ValidationError.oneOfMultiple(path, matched.result())
        }
      }

      // not: must NOT validate against the sub-schema
      not.foreach { case (notSchema, notJson) =>
        if (notSchema.isValid(json, remainingDepth - 1)) {
          errors += ValidationError.not(path, notJson)
        }
      }
    }

    // --- isValid helpers: return Boolean with early exit, zero allocations ---

    private def isValidString(s: String): Boolean = {
      if (hasLengthConstraints) {
        val codePointCount = s.codePointCount(0, s.length)
        minLength match {
          case Some(min) if codePointCount < min => return false
          case _                                 => ()
        }
        maxLength match {
          case Some(max) if codePointCount > max => return false
          case _                                 => ()
        }
      }

      pattern match {
        case Some(p) if !p.matches(s) => return false
        case _                        => ()
      }

      format match {
        case Some(cf) if !cf.validator.validate(s) => return false
        case _                                     => ()
      }

      true
    }

    // isValid fast-path: avoids n.toBigDecimal (BiggerDecimal.fromLong) for Long-valued JSON numbers.
    // Uses double arithmetic when minimumD/maximumD are exact doubles; only allocates BigDecimal
    // when the constraint cannot be represented exactly (NaN sentinel) or the value is non-Long.
    private def isValidNumberRaw(n: JsonNumber): Boolean =
      n.toLong match {
        case Some(v) =>
          val vd = v.toDouble
          // minimumD is NaN when minimum is absent or not exactly a double
          if (!minimumD.isNaN) {
            if (exclusiveMinimum) { if (vd <= minimumD) return false }
            else { if (vd < minimumD) return false }
          } else
            minimum match {
              case Some(min) if exclusiveMinimum && BigDecimal(v) <= min => return false
              case Some(min) if !exclusiveMinimum && BigDecimal(v) < min => return false
              case _                                                     =>
            }

          if (!maximumD.isNaN) {
            if (exclusiveMaximum) { if (vd >= maximumD) return false }
            else { if (vd > maximumD) return false }
          } else
            maximum match {
              case Some(max) if exclusiveMaximum && BigDecimal(v) >= max => return false
              case Some(max) if !exclusiveMaximum && BigDecimal(v) > max => return false
              case _                                                     =>
            }

          multipleOf match {
            case Some(divisor) =>
              val q = vd / divisor.toDouble
              if (q.isInfinite || q.isNaN || q != Math.floor(q)) return false
            case None => ()
          }
          true
        case None =>
          // Non-Long value (rare: large BigDecimal JSON numbers): BigDecimal path
          isValidNumber(n.toBigDecimal.getOrElse(BigDecimal(n.toDouble)))
      }

    private def isValidNumber(n: BigDecimal): Boolean = {
      minimum match {
        case Some(min) if exclusiveMinimum && n <= min => return false
        case Some(min) if !exclusiveMinimum && n < min => return false
        case _                                         => ()
      }
      maximum match {
        case Some(max) if exclusiveMaximum && n >= max => return false
        case Some(max) if !exclusiveMaximum && n > max => return false
        case _                                         => ()
      }
      multipleOf match {
        case Some(divisor) =>
          val quotient = n.toDouble / divisor.toDouble
          if (quotient.isInfinite || quotient.isNaN || quotient != Math.floor(quotient)) return false
        case None => ()
      }
      true
    }

    private def isValidArray(arr: Vector[Json], remainingDepth: Int): Boolean = {
      minItems match {
        case Some(min) if arr.length < min => return false
        case _                             => ()
      }
      maxItems match {
        case Some(max) if arr.length > max => return false
        case _                             => ()
      }

      if (uniqueItems && arr.length > 1) {
        val seen = new mutable.HashSet[Json]()
        var idx = 0
        while (idx < arr.length) {
          if (!seen.add(arr(idx))) return false
          idx += 1
        }
      }

      items match {
        case Some(ItemsConstraint.Single(schema)) =>
          var idx = 0
          while (idx < arr.length) {
            if (!schema.isValid(arr(idx), remainingDepth - 1)) return false
            idx += 1
          }
        case Some(ItemsConstraint.Tuple(schemas)) =>
          val len = math.min(arr.length, schemas.length)
          var idx = 0
          while (idx < len) {
            if (!schemas(idx).isValid(arr(idx), remainingDepth - 1)) return false
            idx += 1
          }
          // additionalItems: validate items beyond tuple length
          additionalItems match {
            case Some(addSchema) =>
              var extraIdx = schemas.length
              while (extraIdx < arr.length) {
                if (!addSchema.isValid(arr(extraIdx), remainingDepth - 1)) return false
                extraIdx += 1
              }
            case None => ()
          }
        case None =>
          // When items is absent, additionalItems has no effect (per spec)
          ()
      }

      true
    }

    private def isValidObject(obj: JsonObject, remainingDepth: Int): Boolean = {
      val keyIter = obj.keys.iterator
      while (keyIter.hasNext) {
        val key = keyIter.next()
        val value = JsonObjectUnsafe.getValue(obj, key)

        val propSchema = properties.get(key) // null if not found — no Option boxing
        val inProperties = propSchema != null
        if (inProperties) {
          if (!propSchema.isValid(value, remainingDepth - 1)) return false
        }

        var matchedPattern = false
        if (hasPatternProperties) {
          var ppIdx = 0
          while (ppIdx < patternPropertiesSize) {
            val (cp, schema) = patternProperties(ppIdx)
            if (cp.matches(key)) {
              matchedPattern = true
              if (!schema.isValid(value, remainingDepth - 1)) return false
            }
            ppIdx += 1
          }
        }

        if (!inProperties && !matchedPattern) {
          additionalProperties match {
            case Some(AdditionalProperties.Forbidden) => return false
            case Some(AdditionalProperties.Allowed(schema)) =>
              if (!schema.isValid(value, remainingDepth - 1)) return false
            case None => ()
          }
        }
      }

      // Required
      var reqIdx = 0
      while (reqIdx < requiredSize) {
        if (!obj.contains(required(reqIdx))) return false
        reqIdx += 1
      }

      // Dependencies
      var depIdx = 0
      while (depIdx < dependenciesSize) {
        val (prop, dep) = dependencies(depIdx)
        if (obj.contains(prop)) {
          dep match {
            case Left(requiredProps) =>
              var rpIdx = 0
              while (rpIdx < requiredProps.length) {
                if (!obj.contains(requiredProps(rpIdx))) return false
                rpIdx += 1
              }
            case Right(schema) =>
              if (!schema.isValid(Json.fromJsonObject(obj), remainingDepth - 1)) return false
          }
        }
        depIdx += 1
      }

      // Property count
      minProperties match {
        case Some(min) if obj.size < min => return false
        case _                           => ()
      }
      maxProperties match {
        case Some(max) if obj.size > max => return false
        case _                           => ()
      }

      true
    }

    private def isValidComposition(json: Json, remainingDepth: Int): Boolean = {
      // anyOf: short-circuit on first match
      if (anyOfSize > 0) {
        var matched = false
        var i = 0
        while (i < anyOfSize && !matched) {
          val (s, _) = anyOf(i)
          if (s.isValid(json, remainingDepth - 1)) matched = true
          i += 1
        }
        if (!matched) return false
      }

      // oneOf: short-circuit after finding 2 matches
      if (oneOfSize > 0) {
        var matchCount = 0
        var i = 0
        while (i < oneOfSize) {
          val (s, _) = oneOf(i)
          if (s.isValid(json, remainingDepth - 1)) {
            matchCount += 1
            if (matchCount > 1) return false
          }
          i += 1
        }
        if (matchCount != 1) return false
      }

      // not: must NOT validate against the sub-schema
      not match {
        case Some((notSchema, _)) if notSchema.isValid(json, remainingDepth - 1) => return false
        case _                                                                   => ()
      }

      true
    }
  }

  /** Type constraint: single type or union of types */
  sealed trait TypeConstraint
  object TypeConstraint {
    case class Single(t: JsonType) extends TypeConstraint
    // mask: bitwise OR of ts.map(_.flag) — precomputed at compile time.
    // Union check: (mask & actualType.matchMask) != 0 — no Set allocation, no lambda.
    case class Union(ts: Set[JsonType], mask: Int) extends TypeConstraint
  }

  /** JSON types */
  sealed trait JsonType {
    def name: String
    // Single-bit flag for bitmask operations.
    def flag: Int
    // Mask to check against a union bitmask for this actual type.
    // Integer.matchMask includes Number.flag because a JSON integer satisfies type:number.
    def matchMask: Int
  }
  object JsonType {
    case object Null extends JsonType { val name = "null"; val flag = 1; val matchMask = 1 }
    case object Boolean extends JsonType { val name = "boolean"; val flag = 2; val matchMask = 2 }
    case object Integer extends JsonType { val name = "integer"; val flag = 4; val matchMask = 4 | 8 }
    case object Number extends JsonType { val name = "number"; val flag = 8; val matchMask = 8 }
    case object String extends JsonType { val name = "string"; val flag = 16; val matchMask = 16 }
    case object Array extends JsonType { val name = "array"; val flag = 32; val matchMask = 32 }
    case object Object extends JsonType { val name = "object"; val flag = 64; val matchMask = 64 }

    def of(json: Json): JsonType = json.fold(
      jsonNull = Null,
      jsonBoolean = _ => Boolean,
      jsonNumber = n =>
        if (n.toLong.isDefined) Integer
        else
          n.toBigDecimal match {
            case Some(bd) if bd.isWhole => Integer
            case _                      => Number
          },
      jsonString = _ => String,
      jsonArray = _ => Array,
      jsonObject = _ => Object
    )

    def fromString(s: String): Option[JsonType] = s match {
      case "null"    => Some(Null)
      case "boolean" => Some(Boolean)
      case "integer" => Some(Integer)
      case "number"  => Some(Number)
      case "string"  => Some(String)
      case "array"   => Some(Array)
      case "object"  => Some(Object)
      case _         => None
    }
  }

  /** Items constraint: single schema or tuple validation */
  sealed trait ItemsConstraint
  object ItemsConstraint {
    case class Single(schema: Schema) extends ItemsConstraint
    case class Tuple(schemas: Vector[Schema]) extends ItemsConstraint
  }

  /** Additional properties constraint */
  sealed trait AdditionalProperties
  object AdditionalProperties {
    case object Forbidden extends AdditionalProperties
    case class Allowed(schema: Schema) extends AdditionalProperties
  }

  /** Pre-compiled regex pattern with fast-path detection.
    *
    * fastPath: 0 = use regex (general case), 1 = always matches (.* / ^.*$), 2 = non-empty only (.+)
    */
  case class CompiledPattern private (
      regex: Pattern,
      source: String,
      fastPath: Int
  ) {
    // JSON Schema patterns use partial matching (find), not full matching (matches).
    // Kept small so the JIT can inline this method and scalar-replace the Matcher
    // created in the case-_ branch (which prevents heap allocation for the regex path).
    def matches(s: String): Boolean = fastPath match {
      case 1 => true // .* always matches
      case 2 => s.nonEmpty // .+ requires non-empty
      case _ => regex.matcher(s).find()
    }
  }

  object CompiledPattern {
    def apply(pattern: String): Either[String, CompiledPattern] =
      try {
        val fp = pattern match {
          case ".*" | "^.*$" => 1 // always matches
          case ".+"          => 2 // non-empty
          case _             => 0 // use regex
        }
        Right(new CompiledPattern(Pattern.compile(pattern), pattern, fp))
      } catch {
        case e: Exception => Left(s"Invalid regex pattern: ${e.getMessage}")
      }
  }
}
