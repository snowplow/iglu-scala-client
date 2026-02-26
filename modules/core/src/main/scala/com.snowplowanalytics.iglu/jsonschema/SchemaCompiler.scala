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
import java.util.{HashMap => JHashMap}
import io.circe.{Json, JsonObject}

/**
 * Compiles JSON Schema (as Circe Json) into optimized Schema AST. Compilation happens once per schema, validation happens many times.
 */
object SchemaCompiler {

  case class CompilationError(path: String, message: String)

  def compile(json: Json): Either[CompilationError, Schema] = {
    val ctx = new CompilationContext(json)
    val result = json.fold(
      jsonNull = Left(CompilationError("$", "Schema cannot be null")),
      jsonBoolean = b => Right(if (b) Schema.Empty else Schema.Never),
      jsonNumber = _ => Left(CompilationError("$", "Schema cannot be a number")),
      jsonString = _ => Left(CompilationError("$", "Schema cannot be a string")),
      jsonArray = _ => Left(CompilationError("$", "Schema cannot be an array")),
      jsonObject = compileObject(_, "$", ctx)
    )
    result.foreach(patchRootRef(ctx, _))
    result
  }

  private class CompilationContext(val rootJson: Json) {
    val cache: mutable.Map[String, Schema.Ref] = mutable.Map.empty

    def resolveRef(ref: String, path: String): Either[CompilationError, Schema] = {
      if (ref == "#") {
        // Root ref — return a proxy that will point to the compiled root
        Right(cache.getOrElseUpdate("#", Schema.Ref(Schema.Empty)))
      } else if (ref.startsWith("#/")) {
        cache.get(ref) match {
          case Some(existing) => Right(existing)
          case None           =>
            // Insert a proxy first to handle recursive refs
            val proxy = Schema.Ref(Schema.Empty)
            cache(ref) = proxy
            resolvePointer(ref.substring(2), path).flatMap { targetJson =>
              compileJson(targetJson, ref, this).map { compiled =>
                proxy.target = compiled
                proxy
              }
            }
        }
      } else {
        // Unknown external refs: treat as empty schema (pass-through)
        Right(Schema.Empty)
      }
    }

    private def resolvePointer(pointer: String, path: String): Either[CompilationError, Json] = {
      val segments = pointer.split("/", -1).toVector.map(decodePointerSegment)
      segments.foldLeft[Either[CompilationError, Json]](Right(rootJson)) { case (acc, segment) =>
        acc.flatMap { current =>
          current.asObject.flatMap(_(segment)) match {
            case Some(value) => Right(value)
            case None =>
              current.asArray.flatMap { arr =>
                scala.util
                  .Try(segment.toInt)
                  .toOption
                  .filter(i => i >= 0 && i < arr.size)
                  .map(arr(_))
              } match {
                case Some(value) => Right(value)
                case None =>
                  Left(CompilationError(path, s"Cannot resolve $$ref pointer: #/$pointer"))
              }
          }
        }
      }
    }

    private def decodePointerSegment(segment: String): String = {
      val unescaped = segment.replace("~1", "/").replace("~0", "~")
      java.net.URLDecoder.decode(unescaped, "UTF-8")
    }
  }

  private def compileJson(
    json: Json,
    path: String,
    ctx: CompilationContext
  ): Either[CompilationError, Schema] = {
    json.fold(
      jsonNull = Left(CompilationError(path, "Schema cannot be null")),
      jsonBoolean = b => Right(if (b) Schema.Empty else Schema.Never),
      jsonNumber = _ => Left(CompilationError(path, "Schema cannot be a number")),
      jsonString = _ => Left(CompilationError(path, "Schema cannot be a string")),
      jsonArray = _ => Left(CompilationError(path, "Schema cannot be an array")),
      jsonObject = compileObject(_, path, ctx)
    )
  }

  private def compileObject(
    obj: JsonObject,
    path: String,
    ctx: CompilationContext
  ): Either[CompilationError, Schema] = {
    if (obj.isEmpty) return Right(Schema.Empty)

    // $ref overrides all sibling keywords (Draft-04 spec)
    obj("$ref") match {
      case Some(refJson) =>
        refJson.asString match {
          case Some(ref) => ctx.resolveRef(ref, s"$path.$$ref")
          case None      => Left(CompilationError(s"$path.$$ref", "$$ref must be a string"))
        }
      case None =>
        for {
          types     <- compileType(obj, path)
          `enum`    <- compileEnum(obj, path)
          minLength <- compilePositiveInt(obj, "minLength", path)
          maxLength <- compilePositiveInt(obj, "maxLength", path)
          pattern   <- compilePattern(obj, path)
          format    <- compileFormat(obj, path)
          minimum   <- compileNumber(obj, "minimum", path)
          maximum   <- compileNumber(obj, "maximum", path)
          exclusiveMinimum = obj("exclusiveMinimum").flatMap(_.asBoolean).getOrElse(false)
          exclusiveMaximum = obj("exclusiveMaximum").flatMap(_.asBoolean).getOrElse(false)
          multipleOf           <- compileNumber(obj, "multipleOf", path)
          items                <- compileItems(obj, path, ctx)
          additionalItems      <- compileAdditionalItems(obj, path, ctx)
          minItems             <- compilePositiveInt(obj, "minItems", path)
          maxItems             <- compilePositiveInt(obj, "maxItems", path)
          uniqueItems          <- compileBoolean(obj, "uniqueItems", path).map(_.getOrElse(false))
          properties           <- compileProperties(obj, path, ctx)
          patternProperties    <- compilePatternProperties(obj, path, ctx)
          additionalProperties <- compileAdditionalProperties(obj, path, ctx)
          required             <- compileRequired(obj, path)
          dependencies         <- compileDependencies(obj, path, ctx)
          minProperties        <- compilePositiveInt(obj, "minProperties", path)
          maxProperties        <- compilePositiveInt(obj, "maxProperties", path)
          anyOf                <- compileSchemaArray(obj, "anyOf", path, ctx)
          oneOf                <- compileSchemaArray(obj, "oneOf", path, ctx)
          allOf                <- compileSchemaArray(obj, "allOf", path, ctx)
          notSchema            <- compileNot(obj, path, ctx)
        } yield {
          val compiled = Schema.Compiled(
            types = types,
            `enum` = `enum`,
            enumStrings = `enum`.flatMap { case (_, vec) =>
              val ss = vec.flatMap(_.asString)
              if (ss.length == vec.length) Some(ss.toSet) else None
            },
            minLength = minLength,
            maxLength = maxLength,
            pattern = pattern,
            format = format,
            minimum = minimum,
            maximum = maximum,
            exclusiveMinimum = exclusiveMinimum,
            exclusiveMaximum = exclusiveMaximum,
            minimumD = toExactDouble(minimum),
            maximumD = toExactDouble(maximum),
            multipleOf = multipleOf,
            items = items,
            additionalItems = additionalItems,
            minItems = minItems,
            maxItems = maxItems,
            uniqueItems = uniqueItems,
            properties = properties,
            patternProperties = patternProperties,
            additionalProperties = additionalProperties,
            required = required,
            dependencies = dependencies,
            minProperties = minProperties,
            maxProperties = maxProperties,
            anyOf = anyOf,
            oneOf = oneOf,
            not = notSchema
          )
          if (allOf.nonEmpty) Schema.WithAllOf(compiled, allOf)
          else compiled
        }
    }
  }

  /** After root compilation, patch the root ref proxy (if any) to point to the final schema. */
  private def patchRootRef(ctx: CompilationContext, root: Schema): Unit = {
    ctx.cache.get("#").foreach { ref =>
      ref.target = root
    }
  }

  private def compileType(
    obj: JsonObject,
    path: String
  ): Either[CompilationError, Option[Schema.TypeConstraint]] = {
    obj("type") match {
      case None => Right(None)
      case Some(json) =>
        json.asString match {
          case Some(s) =>
            Schema.JsonType.fromString(s) match {
              case Some(t) => Right(Some(Schema.TypeConstraint.Single(t)))
              case None    => Left(CompilationError(s"$path.type", s"Unknown type: $s"))
            }
          case None =>
            json.asArray match {
              case Some(arr) =>
                val types = arr.flatMap(_.asString).flatMap(Schema.JsonType.fromString)
                if (types.isEmpty)
                  Left(
                    CompilationError(s"$path.type", "Type array is empty or contains invalid types")
                  )
                else {
                  val mask = types.foldLeft(0)(_ | _.flag)
                  Right(Some(Schema.TypeConstraint.Union(types.toSet, mask)))
                }
              case None =>
                Left(CompilationError(s"$path.type", "type must be a string or array of strings"))
            }
        }
    }
  }

  private def compileEnum(
    obj: JsonObject,
    path: String
  ): Either[CompilationError, Option[(Set[Json], Vector[Json])]] = {
    obj("enum") match {
      case None => Right(None)
      case Some(json) =>
        json.asArray match {
          case Some(arr) if arr.nonEmpty => Right(Some((arr.toSet, arr)))
          case Some(_) =>
            Left(CompilationError(s"$path.enum", "enum must have at least one element"))
          case None => Left(CompilationError(s"$path.enum", "enum must be an array"))
        }
    }
  }

  private def compilePositiveInt(
    obj: JsonObject,
    field: String,
    path: String
  ): Either[CompilationError, Option[Int]] = {
    obj(field) match {
      case None => Right(None)
      case Some(json) =>
        json.asNumber.flatMap(_.toInt) match {
          case Some(n) if n >= 0 => Right(Some(n))
          case Some(n) =>
            Left(CompilationError(s"$path.$field", s"$field must be non-negative, got $n"))
          case None => Left(CompilationError(s"$path.$field", s"$field must be an integer"))
        }
    }
  }

  private def compileNumber(
    obj: JsonObject,
    field: String,
    path: String
  ): Either[CompilationError, Option[BigDecimal]] = {
    obj(field) match {
      case None => Right(None)
      case Some(json) =>
        json.asNumber match {
          case Some(n) => Right(Some(n.toBigDecimal.getOrElse(BigDecimal(n.toDouble))))
          case None    => Left(CompilationError(s"$path.$field", s"$field must be a number"))
        }
    }
  }

  private def compileBoolean(
    obj: JsonObject,
    field: String,
    path: String
  ): Either[CompilationError, Option[Boolean]] = {
    obj(field) match {
      case None => Right(None)
      case Some(json) =>
        json.asBoolean match {
          case Some(b) => Right(Some(b))
          case None    => Left(CompilationError(s"$path.$field", s"$field must be a boolean"))
        }
    }
  }

  private def compilePattern(
    obj: JsonObject,
    path: String
  ): Either[CompilationError, Option[Schema.CompiledPattern]] = {
    obj("pattern") match {
      case None => Right(None)
      case Some(json) =>
        json.asString match {
          case Some(s) =>
            Schema.CompiledPattern(s) match {
              case Right(p)  => Right(Some(p))
              case Left(err) => Left(CompilationError(s"$path.pattern", err))
            }
          case None => Left(CompilationError(s"$path.pattern", "pattern must be a string"))
        }
    }
  }

  private def compileFormat(
    obj: JsonObject,
    path: String
  ): Either[CompilationError, Option[Schema.CompiledFormat]] = {
    obj("format") match {
      case None => Right(None)
      case Some(json) =>
        json.asString match {
          case Some(s) =>
            formats.Formats.get(s) match {
              case Some(validator) =>
                Right(Some(Schema.CompiledFormat(s, validator, formats.FormatPatterns.get(s))))
              case None =>
                // Unknown format — silently ignored per spec
                Right(None)
            }
          case None => Left(CompilationError(s"$path.format", "format must be a string"))
        }
    }
  }

  private def compileItems(
    obj: JsonObject,
    path: String,
    ctx: CompilationContext
  ): Either[CompilationError, Option[Schema.ItemsConstraint]] = {
    obj("items") match {
      case None => Right(None)
      case Some(json) =>
        json.asArray match {
          case Some(arr) =>
            val compiled = arr.zipWithIndex.map { case (item, idx) =>
              compileJson(item, s"$path.items[$idx]", ctx)
            }
            sequence(compiled).map(schemas => Some(Schema.ItemsConstraint.Tuple(schemas)))
          case None =>
            compileJson(json, s"$path.items", ctx).map(s => Some(Schema.ItemsConstraint.Single(s)))
        }
    }
  }

  private val EmptyProperties = new JHashMap[String, Schema](0)

  private def compileProperties(
    obj: JsonObject,
    path: String,
    ctx: CompilationContext
  ): Either[CompilationError, JHashMap[String, Schema]] = {
    obj("properties") match {
      case None => Right(EmptyProperties)
      case Some(json) =>
        json.asObject match {
          case Some(propsObj) =>
            val compiled = propsObj.toList.map { case (key, value) =>
              compileJson(value, s"$path.properties.$key", ctx).map(key -> _)
            }
            sequence(compiled).map { pairs =>
              val map =
                new JHashMap[String, Schema](
                  pairs.size * 4 / 3 + 1
                ) // avoid resizing, allocate capacity so that adding all pairs will lead to map being filled by less than 75%
              val i = pairs.iterator
              while (i.hasNext) {
                val (k, v) = i.next()
                val _      = map.put(k, v)
              }
              map
            }
          case None =>
            Left(CompilationError(s"$path.properties", "properties must be an object"))
        }
    }
  }

  private def compilePatternProperties(
    obj: JsonObject,
    path: String,
    ctx: CompilationContext
  ): Either[CompilationError, Vector[(Schema.CompiledPattern, Schema)]] = {
    obj("patternProperties") match {
      case None => Right(Vector.empty)
      case Some(json) =>
        json.asObject match {
          case Some(ppObj) =>
            val compiled = ppObj.toVector.map { case (patternStr, value) =>
              for {
                cp <- Schema
                  .CompiledPattern(patternStr)
                  .left
                  .map(err => CompilationError(s"$path.patternProperties.$patternStr", err))
                schema <- compileJson(value, s"$path.patternProperties.$patternStr", ctx)
              } yield (cp, schema)
            }
            sequence(compiled)
          case None =>
            Left(
              CompilationError(s"$path.patternProperties", "patternProperties must be an object")
            )
        }
    }
  }

  private def compileAdditionalProperties(
    obj: JsonObject,
    path: String,
    ctx: CompilationContext
  ): Either[CompilationError, Option[Schema.AdditionalProperties]] = {
    obj("additionalProperties") match {
      case None => Right(None)
      case Some(json) =>
        json.asBoolean match {
          case Some(false) => Right(Some(Schema.AdditionalProperties.Forbidden))
          case Some(true)  => Right(None) // true means allow any, same as not specified
          case None =>
            json.asObject match {
              case Some(schemaObj) =>
                compileObject(schemaObj, s"$path.additionalProperties", ctx)
                  .map(s => Some(Schema.AdditionalProperties.Allowed(s)))
              case None =>
                Left(
                  CompilationError(
                    s"$path.additionalProperties",
                    "additionalProperties must be a boolean or object"
                  )
                )
            }
        }
    }
  }

  private def compileRequired(
    obj: JsonObject,
    path: String
  ): Either[CompilationError, Vector[String]] = {
    obj("required") match {
      case None => Right(Vector.empty[String])
      case Some(json) =>
        json.asArray match {
          case Some(arr) =>
            val strings = arr.flatMap(_.asString)
            if (strings.length != arr.length) {
              Left(CompilationError(s"$path.required", "required must be an array of strings"))
            } else {
              Right(strings)
            }
          case None =>
            Left(CompilationError(s"$path.required", "required must be an array"))
        }
    }
  }

  private def compileDependencies(
    obj: JsonObject,
    path: String,
    ctx: CompilationContext
  ): Either[CompilationError, Vector[(String, Either[Vector[String], Schema])]] = {
    obj("dependencies") match {
      case None => Right(Vector.empty)
      case Some(json) =>
        json.asObject match {
          case Some(depsObj) =>
            val compiled = depsObj.toVector.map { case (key, value) =>
              value.asArray match {
                case Some(arr) =>
                  // Property dependency: {"key": ["prop1", "prop2"]}
                  val strings = arr.flatMap(_.asString)
                  if (strings.length != arr.length)
                    Left(
                      CompilationError(
                        s"$path.dependencies.$key",
                        "property dependency must be an array of strings"
                      )
                    )
                  else
                    Right((key, Left(strings)))
                case None =>
                  // Schema dependency: {"key": {schema}}
                  compileJson(value, s"$path.dependencies.$key", ctx).map(s => (key, Right(s)))
              }
            }
            sequence(compiled)
          case None =>
            Left(CompilationError(s"$path.dependencies", "dependencies must be an object"))
        }
    }
  }

  // Returns both compiled schema and original JSON for error messages (networknt compatibility)
  private def compileSchemaArray(
    obj: JsonObject,
    field: String,
    path: String,
    ctx: CompilationContext
  ): Either[CompilationError, Vector[(Schema, Json)]] = {
    obj(field) match {
      case None => Right(Vector.empty)
      case Some(json) =>
        json.asArray match {
          case Some(arr) =>
            val compiled = arr.zipWithIndex.map { case (item, idx) =>
              compileJson(item, s"$path.$field[$idx]", ctx).map(schema => (schema, item))
            }
            sequence(compiled)
          case None =>
            Left(CompilationError(s"$path.$field", s"$field must be an array"))
        }
    }
  }

  private def compileNot(
    obj: JsonObject,
    path: String,
    ctx: CompilationContext
  ): Either[CompilationError, Option[(Schema, Json)]] = {
    obj("not") match {
      case None       => Right(None)
      case Some(json) => compileJson(json, s"$path.not", ctx).map(schema => Some((schema, json)))
    }
  }

  private def compileAdditionalItems(
    obj: JsonObject,
    path: String,
    ctx: CompilationContext
  ): Either[CompilationError, Option[Schema]] = {
    obj("additionalItems") match {
      case None => Right(None)
      case Some(json) =>
        json.asBoolean match {
          case Some(false) => Right(Some(Schema.AdditionalItemsForbidden))
          case Some(true)  => Right(None) // true means allow any, same as not specified
          case None =>
            compileJson(json, s"$path.additionalItems", ctx).map(Some(_))
        }
    }
  }

  private def sequence[A](
    eithers: Vector[Either[CompilationError, A]]
  ): Either[CompilationError, Vector[A]] = {
    val builder = Vector.newBuilder[A]
    builder.sizeHint(eithers.length)
    var i = 0
    while (i < eithers.length) {
      eithers(i) match {
        case Left(err) => return Left(err)
        case Right(a)  => builder += a
      }
      i += 1
    }
    Right(builder.result())
  }

  /**
   * Returns an exact double representation of the BigDecimal, or Double.NaN if absent, non-integer, or not exactly representable as a
   * double.
   *
   * Uses a zero-allocation integer-only check: `bd.isValidLong` (confirms whole number in Long range) + `v.toDouble.toLong == v` (confirms
   * no precision loss in double conversion). Fractional bounds (e.g. 0.5) return NaN and fall back to BigDecimal at validation time —
   * acceptable since all real-world Snowplow and Draft-04 meta-schema bounds are integers.
   */
  private def toExactDouble(opt: Option[BigDecimal]): Double = opt match {
    case None => Double.NaN
    case Some(bd) =>
      if (bd.isValidLong) {
        val v = bd.toLong // exact: isValidLong guarantees no truncation
        val d = v.toDouble
        if (d.toLong == v) d else Double.NaN // NaN when |v| > 2^53 (imprecise double)
      } else Double.NaN
  }

  private def sequence[A](
    eithers: List[Either[CompilationError, A]]
  ): Either[CompilationError, List[A]] = {
    val builder = List.newBuilder[A]
    val iter    = eithers.iterator
    while (iter.hasNext) {
      iter.next() match {
        case Left(err) => return Left(err)
        case Right(a)  => builder += a
      }
    }
    Right(builder.result())
  }
}
