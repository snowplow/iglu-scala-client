/*
 * Copyright (c) 2014-2023 Snowplow Analytics Ltd. All rights reserved.
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
package com.snowplowanalytics.iglu.client.validator

import com.fasterxml.jackson.databind.ObjectMapper

import org.specs2.Specification

class CustomMessageFormatterSpec extends Specification {
  def is = s2"""
  CustomMessageFormatter.transformArguments should
    transform enum arguments by removing quotes            $enumTransform
    transform enum non-array fallback                      $enumNonArray
    transform enum with empty arguments                    $enumEmpty
    drop third argument for format validator               $formatTransform
    keep format with 2 args                                $formatKeepTwo
    transform not arguments                                $notTransform
    transform oneOf with schema node                       $oneOfTransform
    pass through unknown validator                         $unknownValidator
    pass through when no arguments                         $noArgs
    handle oneOf without array schema node                 $oneOfNoArray
    handle oneOf with insufficient arguments               $oneOfInsufficientArgs
  CustomMessageFormatter.formatMessage should
    format additionalItems specially                        $additionalItems
    format a known validator type                          $knownValidator
    return None for unknown validator type                 $unknownValidatorFormat
    format required validator                              $requiredValidator
  """

  val mapper = new ObjectMapper()

  def enumTransform = {
    val args   = Seq("""["foo", "bar", "baz"]""")
    val result = CustomMessageFormatter.transformArguments("enum", args)
    result must_== Seq("[foo, bar, baz]")
  }

  def enumNonArray = {
    val args   = Seq("not-json")
    val result = CustomMessageFormatter.transformArguments("enum", args)
    // Fallback to original when parse fails
    result must_== Seq("not-json")
  }

  def enumEmpty = {
    val args   = Seq.empty[String]
    val result = CustomMessageFormatter.transformArguments("enum", args)
    result must_== Seq.empty[String]
  }

  def formatTransform = {
    val args   = Seq("date-time", "^\\d{4}-\\d{2}-\\d{2}$", "bad-value")
    val result = CustomMessageFormatter.transformArguments("format", args)
    result must_== Seq("date-time", "^\\d{4}-\\d{2}-\\d{2}$")
  }

  def formatKeepTwo = {
    val args   = Seq("date-time", "^\\d{4}$")
    val result = CustomMessageFormatter.transformArguments("format", args)
    result must_== Seq("date-time", "^\\d{4}$")
  }

  def notTransform = {
    val args   = Seq(""""$.not" : {"type":"string"}""")
    val result = CustomMessageFormatter.transformArguments("not", args)
    result must_== Seq(""""not" : {"type":"string"}""")
  }

  def oneOfTransform = {
    val schemaNode = mapper.readTree("""[{"type":"string"},{"type":"number"},{"type":"boolean"}]""")
    val args       = Seq("3", "0, 1")
    val result     = CustomMessageFormatter.transformArguments("oneOf", args, Some(schemaNode))
    result must_== List("""{"type":"string"}{"type":"number"}""")
  }

  def unknownValidator = {
    val args   = Seq("arg1", "arg2")
    val result = CustomMessageFormatter.transformArguments("unknownType", args)
    result must_== Seq("arg1", "arg2")
  }

  def noArgs = {
    val result = CustomMessageFormatter.transformArguments("enum", Seq.empty)
    result must_== Seq.empty[String]
  }

  def oneOfNoArray = {
    val schemaNode = mapper.readTree("""{"type":"string"}""")
    val args       = Seq("1", "0")
    val result     = CustomMessageFormatter.transformArguments("oneOf", args, Some(schemaNode))
    // schemaNode is not an array, so the guard fails and falls through to default
    result must_== Seq("1", "0")
  }

  def oneOfInsufficientArgs = {
    val schemaNode = mapper.readTree("""[{"type":"string"}]""")
    val args       = Seq("1")
    val result     = CustomMessageFormatter.transformArguments("oneOf", args, Some(schemaNode))
    // Only 1 argument, guard requires >= 2
    result must_== Seq("1")
  }

  def additionalItems = {
    val result = CustomMessageFormatter.formatMessage("additionalItems", "$", Seq("2"))
    result must beSome("$[2]: no validator found at this index")
  }

  def knownValidator = {
    // "required" is a known template: {0}: required property ''{1}'' not found
    val result = CustomMessageFormatter.formatMessage("required", "$.field", Seq("name"))
    result must beSome[String].which(_.contains("required"))
  }

  def unknownValidatorFormat = {
    val result = CustomMessageFormatter.formatMessage("nonexistent_validator_xyz", "$", Seq.empty)
    result must beNone
  }

  def requiredValidator = {
    val result = CustomMessageFormatter.formatMessage("required", "$", Seq("id"))
    result must beSome[String].which(s => s.contains("id") && s.contains("required"))
  }
}
