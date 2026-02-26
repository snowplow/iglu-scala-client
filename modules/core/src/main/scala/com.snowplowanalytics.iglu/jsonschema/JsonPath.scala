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

/**
 * Represents a path to a location in a JSON document. Used for error reporting.
 */
sealed trait JsonPath {
  def render: String
  def field(name: String): JsonPath = JsonPath.Field(this, name)
  def index(idx: Int): JsonPath     = JsonPath.Index(this, idx)
}

object JsonPath {
  case object Root extends JsonPath {
    def render: String = "$"
  }

  case class Field(parent: JsonPath, name: String) extends JsonPath {
    def render: String = s"${parent.render}.$name"
  }

  case class Index(parent: JsonPath, idx: Int) extends JsonPath {
    def render: String = s"${parent.render}[$idx]"
  }
}
