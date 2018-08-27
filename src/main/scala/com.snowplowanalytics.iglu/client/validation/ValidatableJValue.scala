/*
 * Copyright (c) 2014-2017 Snowplow Analytics Ltd. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */
package com.snowplowanalytics.iglu.client
package validation

// Json4s
import org.json4s.JValue
import org.json4s.jackson.JsonMethods.{asJsonNode, fromJsonNode}

object ValidatableJValue extends Validatable[JValue] {

  def validateAgainstSchema(instance: JValue, schema: JValue): ValidatedNelType[JValue] =
    ValidatableJsonMethods
      .validateAgainstSchema(asJsonNode(instance), asJsonNode(schema))
      .map(fromJsonNode)

  def validate(instance: JValue, dataOnly: Boolean = false)(
    implicit resolver: Resolver): ValidatedNelType[JValue] =
    ValidatableJsonMethods.validate(asJsonNode(instance), dataOnly)(resolver).map(fromJsonNode)

  def validateAndIdentifySchema(instance: JValue, dataOnly: Boolean = false)(
    implicit resolver: Resolver): ValidatedNelType[(SchemaKey, JValue)] = {
    ValidatableJsonMethods.validateAndIdentifySchema(asJsonNode(instance), dataOnly)(resolver).map {
      case (key, json) =>
        (key, fromJsonNode(json))
    }
  }

  def verifySchemaAndValidate(json: JValue, schemaCriterion: SchemaCriterion, dataOnly: Boolean)(
    implicit resolver: Resolver): ValidatedNelType[JValue] =
    ValidatableJsonMethods
      .verifySchemaAndValidate(asJsonNode(json), schemaCriterion, dataOnly)(resolver)
      .map(fromJsonNode)
}
