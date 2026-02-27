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

import io.circe.Json

/** Main entry point for JSON Schema validation. Thread-safe, reusable.
  */
object Validator {

  /** Pre-compiled Draft-04 meta-schema. Compiled once, used to validate user schemas. */
  lazy val Draft04MetaSchema: CompiledSchema = {
    val stream = getClass.getResourceAsStream("/com/snowplowanalytics/iglu/jsonschema/draft-04-schema.json")
    val source = scala.io.Source.fromInputStream(stream, "UTF-8")
    val json =
      try io.circe.parser.parse(source.mkString).fold(e => throw e, identity)
      finally source.close()
    SchemaCompiler
      .compile(json)
      .fold(
        e => throw new RuntimeException(s"Failed to compile Draft-04 meta-schema: ${e.message}"),
        CompiledSchema(_)
      )
  }

  /** Validate a schema against the Draft-04 meta-schema.
    *
    * @param schema
    *   The JSON Schema to validate
    * @return
    *   Vector of validation errors (empty if the schema is valid Draft-04)
    */
  def checkSchema(schema: Json): Vector[ValidationError] =
    Draft04MetaSchema.validate(schema)

  /** Compile a JSON Schema for repeated use. This should be done once per schema and the result cached.
    *
    * @param schema
    *   The JSON Schema as Circe Json
    * @return
    *   Either a compilation error or a compiled schema ready for validation
    */
  def compile(schema: Json): Either[SchemaCompiler.CompilationError, CompiledSchema] =
    SchemaCompiler.compile(schema).map(CompiledSchema(_))

  /** Validate JSON data against a raw schema. Note: This compiles the schema on each call. For better performance, use `compile` to get a
    * `CompiledSchema` and reuse it.
    *
    * @param data
    *   The JSON data to validate
    * @param schema
    *   The JSON Schema to validate against
    * @return
    *   Either a compilation error or a list of validation errors (empty if valid)
    */
  def validate(data: Json, schema: Json): Either[SchemaCompiler.CompilationError, Vector[ValidationError]] =
    compile(schema).map(_.validate(data))

  /** Check if JSON data is valid against a raw schema. Note: This compiles the schema on each call. For better performance, use `compile`
    * to get a `CompiledSchema` and reuse it.
    *
    * @param data
    *   The JSON data to validate
    * @param schema
    *   The JSON Schema to validate against
    * @return
    *   Either a compilation error or a boolean indicating validity
    */
  def isValid(data: Json, schema: Json): Either[SchemaCompiler.CompilationError, Boolean] =
    compile(schema).map(_.isValid(data))
}

/** Compiled schema ready for validation. Immutable, thread-safe, reusable.
  *
  * @param schema
  *   The internal compiled schema representation
  */
case class CompiledSchema(private val schema: Schema) {

  /** Default maximum JSON depth (matches iglu-scala-client default) */
  private val DefaultMaxDepth = 100

  /** Validate JSON data against this schema.
    *
    * @param data
    *   The JSON data to validate
    * @return
    *   Vector of validation errors (empty if valid)
    */
  def validate(data: Json): Vector[ValidationError] =
    validate(data, DefaultMaxDepth)

  /** Validate JSON data against this schema with depth limit.
    *
    * @param data
    *   The JSON data to validate
    * @param maxDepth
    *   Maximum nesting depth allowed (prevents stack overflow from deeply nested JSON)
    * @return
    *   Vector of validation errors (empty if valid)
    */
  def validate(data: Json, maxDepth: Int): Vector[ValidationError] =
    schema.validate(data, JsonPath.Root, maxDepth)

  /** Check if JSON data is valid against this schema. Uses dedicated fast path with short-circuit evaluation.
    *
    * @param data
    *   The JSON data to validate
    * @return
    *   true if valid, false otherwise
    */
  def isValid(data: Json): Boolean =
    schema.isValid(data, DefaultMaxDepth)

  /** Check if JSON data is valid against this schema with depth limit. Uses dedicated fast path with short-circuit evaluation.
    *
    * @param data
    *   The JSON data to validate
    * @param maxDepth
    *   Maximum nesting depth allowed
    * @return
    *   true if valid, false otherwise
    */
  def isValid(data: Json, maxDepth: Int): Boolean =
    schema.isValid(data, maxDepth)
}
