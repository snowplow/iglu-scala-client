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

/** Represents a validation error with path and message. Compatible with networknt json-schema-validator error format.
  *
  * @param path
  *   The JSON path where the error occurred (for required/additionalProperties, this is the parent path)
  * @param keyword
  *   The JSON Schema keyword that failed
  * @param message
  *   The error message (without path prefix)
  * @param targets
  *   Additional context values for the error (types, property names, limits, etc.)
  */
case class ValidationError(
    path: JsonPath,
    keyword: String,
    message: String,
    targets: List[String] = List.empty
) {
  def render: String = s"${path.render}: $message"
}

object ValidationError {
  // IPv4 pattern matching networknt format exactly for test compatibility
  private[jsonschema] val Ipv4Pattern =
    "^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])$"

  def typeMismatch(path: JsonPath, actual: String, expected: String): ValidationError =
    ValidationError(path, "type", s"$actual found, $expected expected", List(actual, expected))

  def typeMismatchUnion(path: JsonPath, actual: String, expected: Set[String]): ValidationError =
    ValidationError(path, "type", s"$actual found, [${expected.mkString(", ")}] expected", actual :: expected.toList)

  def `enum`(path: JsonPath, allowed: List[String]): ValidationError = {
    val rendered = allowed.mkString(", ")
    ValidationError(path, "enum", s"does not have a value in the enumeration [$rendered]", allowed)
  }

  def minLength(path: JsonPath, min: Int): ValidationError =
    ValidationError(path, "minLength", s"must be at least $min characters long", List(min.toString))

  def maxLength(path: JsonPath, max: Int): ValidationError =
    ValidationError(path, "maxLength", s"may only be $max characters long", List(max.toString))

  def pattern(path: JsonPath, pattern: String): ValidationError =
    ValidationError(path, "pattern", s"does not match the regex pattern $pattern", List(pattern))

  def format(path: JsonPath, format: String, pattern: String): ValidationError =
    ValidationError(path, "format", s"does not match the $format pattern $pattern", List(format, pattern))

  def minimum(path: JsonPath, min: BigDecimal): ValidationError =
    ValidationError(path, "minimum", s"must have a minimum value of $min", List(min.toString))

  def maximum(path: JsonPath, max: BigDecimal): ValidationError =
    ValidationError(path, "maximum", s"must have a maximum value of $max", List(max.toString))

  def multipleOf(path: JsonPath, divisor: BigDecimal): ValidationError =
    ValidationError(path, "multipleOf", s"must be multiple of $divisor", List(divisor.toString))

  def minItems(path: JsonPath, min: Int): ValidationError =
    ValidationError(path, "minItems", s"there must be a minimum of $min items in the array", List(min.toString))

  def maxItems(path: JsonPath, max: Int): ValidationError =
    ValidationError(path, "maxItems", s"there must be a maximum of $max items in the array", List(max.toString))

  def uniqueItems(path: JsonPath): ValidationError =
    ValidationError(path, "uniqueItems", s"the items in the array must be unique", List.empty)

  // For required/additionalProperties: path is the PARENT, message includes the CHILD path.
  // render produces "$.child: message text" (child path prefix, not parent).
  def required(parentPath: JsonPath, property: String): ValidationError = {
    val messagePath = parentPath.field(property)
    ValidationError(parentPath, "required", s"${messagePath.render}: is missing but it is required", List(property))
  }

  def additionalProperty(parentPath: JsonPath, property: String): ValidationError = {
    val messagePath = parentPath.field(property)
    ValidationError(
      parentPath,
      "additionalProperties",
      s"${messagePath.render}: is not defined in the schema and the schema does not allow additional properties",
      List(property)
    )
  }

  def minProperties(path: JsonPath, min: Int): ValidationError =
    // networknt format: "should have a minimum of"
    ValidationError(path, "minProperties", s"should have a minimum of $min properties", List(min.toString))

  def maxProperties(path: JsonPath, max: Int): ValidationError =
    // networknt format: "may only have a maximum of"
    ValidationError(path, "maxProperties", s"may only have a maximum of $max properties", List(max.toString))

  def oneOfNone(path: JsonPath, count: Int): ValidationError =
    ValidationError(path, "oneOf", s"does not match any of the $count schemas in oneOf", List.empty)

  def oneOfMultiple(path: JsonPath, matchedSchemas: Vector[io.circe.Json]): ValidationError = {
    val schemasStr = matchedSchemas.map(_.noSpaces).mkString
    ValidationError(
      path,
      "oneOf",
      s"should be valid to one and only one of schema, but more than one are valid: $schemasStr",
      List.empty
    )
  }

  def dependency(path: JsonPath, property: String, requiredProperties: Vector[String]): ValidationError =
    ValidationError(
      path,
      "dependencies",
      s"has an error with dependencies {$property=[${requiredProperties.mkString(", ")}]}",
      List.empty
    )

  def additionalItems(path: JsonPath): ValidationError =
    ValidationError(path, "additionalItems", "no validator found at this index", List.empty)

  def not(path: JsonPath, notSchemaJson: io.circe.Json): ValidationError =
    ValidationError(path, "not", s"""should not be valid to the schema "not" : ${notSchemaJson.noSpaces}""", List.empty)

  def maxDepthExceeded(path: JsonPath): ValidationError =
    ValidationError(path, "maxDepth", "Maximum allowed JSON depth exceeded", List.empty)
}
