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

/**
 * Compiled schema representation. Immutable, reusable, optimized for repeated validation.
 */
sealed trait Schema {

  /**
   * Validate JSON data against this schema.
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

  /**
   * Fast validity check with short-circuit evaluation. No error construction, no path allocation.
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
    def validate(json: Json, path: JsonPath, remainingDepth: Int): Vector[ValidationError] =
      Vector.empty
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
    def validate(json: Json, path: JsonPath, remainingDepth: Int): Vector[ValidationError] =
      base.validate(json, path, remainingDepth) ++
        allOf.flatMap { case (s, _) => s.validate(json, path, remainingDepth - 1) }
    def isValid(json: Json, remainingDepth: Int): Boolean =
      base.isValid(json, remainingDepth) && allOf.forall { case (s, _) =>
        s.isValid(json, remainingDepth - 1)
      }
  }

  /** Pre-resolved format validator with cached error pattern (no Map.get per validation) */
  case class CompiledFormat(name: String, validator: formats.Format, errorPattern: String)

  /**
   * Composite schema with all keyword validations. Uses HashMap for O(1) property lookup (instead of SortedMap's O(log n)).
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
    patternProperties: Vector[(CompiledPattern, Schema)],
    additionalProperties: Option[AdditionalProperties],
    required: Vector[String],
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
    private val hasRequired: Boolean          = required.nonEmpty
    private val hasDependencies: Boolean      = dependencies.nonEmpty
    private val hasComposition: Boolean       = anyOf.nonEmpty || oneOf.nonEmpty || not.isDefined
    // Pre-computed sizes: avoid Vector.length virtual dispatch in while-loop bounds
    private val patternPropertiesSize: Int = patternProperties.length
    private val requiredSize: Int          = required.length
    private val dependenciesSize: Int      = dependencies.length
    private val anyOfSize: Int             = anyOf.length
    private val oneOfSize: Int             = oneOf.length

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
                errors += ValidationError.typeMismatchUnion(
                  path,
                  actualType.name,
                  expectedTypes.map(_.name)
                )
          }
        case None => ()
      }

      // Enum validation — string fast-path avoids Json.hashCode
      `enum` match {
        case Some((allowedSet, allowedVec)) =>
          val inEnum = enumStrings match {
            case Some(ss) =>
              val s = JsonObjectUnsafe.stringValue(json) // null if not a JString
              s != null && ss.contains(s)
            case None => allowedSet.contains(json)
          }
          if (!inEnum) {
            val allowedStrs = allowedVec.map { v =>
              v.asString.getOrElse(v.noSpaces)
            }.toList
            errors += ValidationError.`enum`(path, allowedStrs)
          }
        case None => ()
      }

      // Type-specific validations — use json dispatch, operate directly on unwrapped values
      json.fold(
        jsonNull = (),
        jsonBoolean = _ => (),
        jsonNumber = n =>
          if (hasNumberConstraints) {
            val bd = n.toBigDecimal.getOrElse(BigDecimal(n.toDouble))
            validateNumber(bd, path, errors)
          },
        jsonString = s => if (hasStringConstraints) validateString(s, path, errors),
        jsonArray = a => validateArray(a, path, remainingDepth, errors),
        jsonObject = o => validateObject(o, path, remainingDepth, errors)
      )

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

      // Type-specific validations + composition
      json.fold(
        jsonNull = true,
        jsonBoolean = _ => true,
        jsonNumber = n =>
          if (hasNumberConstraints) isValidNumberRaw(n)
          else true,
        jsonString = s =>
          if (hasStringConstraints) isValidString(s)
          else true,
        jsonArray = a => isValidArray(a, remainingDepth),
        jsonObject = o => isValidObject(o, remainingDepth)
      ) && (!hasComposition || isValidComposition(json, remainingDepth))
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
      multipleOf.foreach { divisor =>
        if (divisor != BigDecimal(0)) {
          val quotient = n.toDouble / divisor.toDouble
          if (quotient.isInfinite || quotient.isNaN || quotient != Math.floor(quotient)) {
            errors += ValidationError.multipleOf(path, divisor)
          }
        }
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
        var idx  = 0
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
        val key   = keyIter.next()
        val value = JsonObjectUnsafe.getValue(obj, key) // safe: key came from obj.keys

        val propSchema   = properties.get(key) // null if not found — no Option boxing
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
                var rpIdx      = 0
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
        var i          = 0
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
        var i          = 0
        while (i < oneOfSize) {
          if (oneOf(i)._1.isValid(json, remainingDepth - 1)) matchCount += 1
          i += 1
        }
        if (matchCount == 0) {
          errors += ValidationError.oneOfNone(path, oneOf.length)
        } else if (matchCount > 1) {
          // Only build the matched schemas vector for the error message
          val matched = Vector.newBuilder[Json]
          var j       = 0
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
            case Some(divisor) if divisor != BigDecimal(0) =>
              val q = vd / divisor.toDouble
              if (q.isInfinite || q.isNaN || q != Math.floor(q)) return false
            case _ => ()
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
        case Some(divisor) if divisor != BigDecimal(0) =>
          val quotient = n.toDouble / divisor.toDouble
          if (quotient.isInfinite || quotient.isNaN || quotient != Math.floor(quotient))
            return false
        case _ => ()
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
        var idx  = 0
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
        val key   = keyIter.next()
        val value = JsonObjectUnsafe.getValue(obj, key) // safe: key came from obj.keys

        val propSchema   = properties.get(key) // null if not found — no Option boxing
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
        var i       = 0
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
        var i          = 0
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
    case object Null    extends JsonType { val name = "null"; val flag = 1; val matchMask = 1    }
    case object Boolean extends JsonType { val name = "boolean"; val flag = 2; val matchMask = 2 }
    case object Integer extends JsonType {
      val name = "integer"; val flag = 4; val matchMask = 4 | 8
    }
    case object Number extends JsonType { val name = "number"; val flag = 8; val matchMask = 8   }
    case object String extends JsonType { val name = "string"; val flag = 16; val matchMask = 16 }
    case object Array  extends JsonType { val name = "array"; val flag = 32; val matchMask = 32  }
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
    case class Single(schema: Schema)         extends ItemsConstraint
    case class Tuple(schemas: Vector[Schema]) extends ItemsConstraint
  }

  /** Additional properties constraint */
  sealed trait AdditionalProperties
  object AdditionalProperties {
    case object Forbidden              extends AdditionalProperties
    case class Allowed(schema: Schema) extends AdditionalProperties
  }

  /**
   * Pre-compiled regex pattern with fast-path detection.
   *
   * fastPath constants: 0 = use regex (general case) 1 = always matches (.* / ^.*$) 2 = non-empty only (.+) 3 = anchored char-class scan:
   * ^prefix?[class]quant$ — no Matcher allocation
   *
   * Fields charSet/csMinLen/csMaxLen/csPrefix are only valid when fastPath == 3.
   */
  case class CompiledPattern private (
    regex: Pattern,
    source: String,
    fastPath: Int,
    // fastPath == 3 fields (null / 0 otherwise):
    charSet: Array[Boolean], // 128-element ASCII char bitmap
    csMinLen: Int,           // min length of the char-class portion (after prefix)
    csMaxLen: Int,           // max length; -1 = unbounded
    csPrefix: String         // literal prefix before the char class; "" = none
  ) {
    // JSON Schema patterns use partial matching (find), not full matching (matches).
    // Kept small so the JIT can inline this method and scalar-replace the Matcher
    // created in the case-_ branch (which prevents heap allocation for the regex path).
    def matches(s: String): Boolean = fastPath match {
      case 1 => true         // .* always matches
      case 2 => s.nonEmpty   // .+ requires non-empty
      case 3 => matchesCC(s) // anchored char-class scan — see below
      case _ => regex.matcher(s).find()
    }

    // Anchored char-class fast path: no Matcher/StringBuilder allocation.
    // csPrefix is a literal prefix the string must start with.
    // The remainder must have length in [csMinLen, csMaxLen] and every char
    // must appear in the 128-element charSet bitmap.
    // Kept in a separate method so that `matches` stays small enough to inline.
    private def matchesCC(s: String): Boolean = {
      val plen = csPrefix.length
      val slen = s.length
      val clen = slen - plen
      if (clen < csMinLen) false
      else if (csMaxLen >= 0 && clen > csMaxLen) false
      else if (plen > 0 && !s.startsWith(csPrefix)) false
      else {
        val cs = charSet
        var i  = plen
        var ok = true
        while (ok && i < slen) {
          val c = s.charAt(i)
          ok = c < 128 && cs(c.toInt)
          i += 1
        }
        ok
      }
    }
  }
  object CompiledPattern {
    def apply(pattern: String): Either[String, CompiledPattern] =
      try {
        val compiled = Pattern.compile(pattern)
        pattern match {
          case ".*" | "^.*$" =>
            Right(new CompiledPattern(compiled, pattern, 1, null, 0, 0, null))
          case ".+" =>
            Right(new CompiledPattern(compiled, pattern, 2, null, 0, 0, null))
          case _ =>
            detectCharClass(pattern) match {
              case Some((cs, minLen, maxLen, prefix)) =>
                Right(new CompiledPattern(compiled, pattern, 3, cs, minLen, maxLen, prefix))
              case None =>
                Right(new CompiledPattern(compiled, pattern, 0, null, 0, 0, null))
            }
        }
      } catch {
        case e: Exception => Left(s"Invalid regex pattern: ${e.getMessage}")
      }

    /**
     * Recognise anchored single-char-class patterns of the form: ^[literal_prefix]?[char_class][quantifier]$ where:
     *   - prefix : zero or more non-meta literal ASCII characters
     *   - char_class : `[...]` (no negation `^`, no `\` escapes, ASCII-only)
     *   - quantifier : `+`, `*`, `?`, `{n}`, `{n,}`, `{n,m}`, or implicit `{1,1}`
     *
     * Returns Some((charSet, minLen, maxLen, prefix)) on success, None otherwise. On None the caller falls back to full regex.
     */
    private def detectCharClass(pattern: String): Option[(Array[Boolean], Int, Int, String)] = {
      val len = pattern.length
      if (len < 4) return None
      if (pattern.charAt(0) != '^') return None
      if (pattern.charAt(len - 1) != '$') return None

      var i = 1 // skip '^'

      // Parse optional literal prefix: non-meta, non-'[' ASCII chars.
      val prefixStart = i
      while (i < len - 1 && pattern.charAt(i) != '[' && !isMeta(pattern.charAt(i)))
        i += 1
      val prefix = pattern.substring(prefixStart, i)

      // Must arrive at '['
      if (i >= len - 1 || pattern.charAt(i) != '[') return None
      i += 1 // skip '['

      // Reject negated classes [^...]
      if (i >= len - 1 || pattern.charAt(i) == '^') return None

      val charSet    = new Array[Boolean](128)
      val classStart = i

      // Parse char-class body up to ']'
      while (i < len - 1 && pattern.charAt(i) != ']') {
        val c = pattern.charAt(i)
        if (c >= 128) return None  // non-ASCII
        if (c == '\\') return None // escape sequences not supported
        // Range a-z: current char, '-', then a non-']' char
        if (
          i + 2 < len &&
          pattern.charAt(i + 1) == '-' &&
          pattern.charAt(i + 2) != ']'
        ) {
          val from = c.toInt
          val to   = pattern.charAt(i + 2).toInt
          if (to >= 128 || from > to) return None
          var j = from
          while (j <= to) { charSet(j) = true; j += 1 }
          i += 3
        } else {
          charSet(c.toInt) = true
          i += 1
        }
      }

      if (i >= len - 1 || pattern.charAt(i) != ']') return None // unclosed class
      if (i == classStart) return None                          // empty class []
      i += 1                                                    // skip ']'

      // Parse quantifier, or treat missing quantifier as implicit {1,1}
      if (i >= len) return None
      val (minLen, maxLen) =
        if (i == len - 1 && pattern.charAt(i) == '$') {
          (1, 1) // no explicit quantifier
        } else {
          pattern.charAt(i) match {
            case '+' => i += 1; (1, -1)
            case '*' => i += 1; (0, -1)
            case '?' => i += 1; (0, 1)
            case '{' =>
              i += 1
              val numStart = i
              while (i < len && pattern.charAt(i) >= '0' && pattern.charAt(i) <= '9') i += 1
              if (i >= len || numStart == i) return None
              val n = pattern.substring(numStart, i).toInt
              pattern.charAt(i) match {
                case '}' =>
                  i += 1
                  (n, n)
                case ',' =>
                  i += 1
                  if (i < len && pattern.charAt(i) == '}') {
                    i += 1
                    (n, -1) // {n,} = at least n
                  } else {
                    val numStart2 = i
                    while (i < len && pattern.charAt(i) >= '0' && pattern.charAt(i) <= '9') i += 1
                    if (i >= len || numStart2 == i || pattern.charAt(i) != '}') return None
                    val m = pattern.substring(numStart2, i).toInt
                    i += 1
                    (n, m)
                  }
                case _ => return None
              }
            case _ => return None
          }
        }

      // Must be sitting exactly on the trailing '$'
      if (i != len - 1 || pattern.charAt(i) != '$') return None

      Some((charSet, minLen, maxLen, prefix))
    }

    private def isMeta(c: Char): Boolean = c match {
      case '.' | '*' | '+' | '?' | '(' | ')' | '{' | '}' | '[' | ']' | '|' | '\\' | '^' | '$' =>
        true
      case _ => false
    }
  }
}
