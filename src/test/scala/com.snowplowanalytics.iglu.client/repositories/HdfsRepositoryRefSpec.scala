/*
 * Copyright (c) 2014 Snowplow Analytics Ltd. All rights reserved.
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
package repositories

// Scalaz
import scalaz._
import Scalaz._

// This project
import validation.ProcessingMessageMethods._

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.scalaz.ValidationMatchers

class HdfsRepositoryRefSpec extends Specification with DataTables with ValidationMatchers { def is =

  "This is a specification to test an hdfs RepositoryRef"                                              ^
                                                                                                          p^
  "a JSON configuration for an hdfs RepositoryRef should be recognized as such"                        ! e1^
  "a JSON configuration can be used to construct an hdfs RepositoryRef"                                ! e2^
  "retrieving an existent JSON Schema from an hdfs RepositoryRef should work"                          ! e3^
  "requesting a non-existent JSON Schema from an hdfs RepositoryRef should return None"                ! e4^
                                                                                                           end

  val AcmeConfig = SpecHelpers.asJValue(
     """|{
          |"name": "Acme Hdfs",
          |"priority": 100,
          |"vendorPrefixes": [ "uk.co.acme", "de.acme" ],
          |"connection": {
            |"hdfs": {
              |"path": "/acme-hdfs-new"
            |}
          |}
        |}""".stripMargin.replaceAll("[\n\r]","")
    )

  def e1 = HdfsRepositoryRef.isHdfs(AcmeConfig) must beTrue

  def e2 = {
    val expected = HdfsRepositoryRef(
      config = RepositoryRefConfig("Acme Hdfs", 100, List("uk.co.acme", "de.acme")),
      path = "/acme-hdfs-new"
    )
    HdfsRepositoryRef.parse(AcmeConfig) must beSuccessful(expected)
  }

  def e3 = {
    val schemaKey = SchemaKey("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", "1-0-0")
    val expected =
      """{
        |"$schema":"http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
        |"description":"Test schema",
        |"self":{
          |"vendor":"com.snowplowanalytics.iglu-test",
          |"name":"stock-item",
          |"format":"jsonschema",
          |"version":"1-0-0"
        |},
        |"type":"object",
        |"properties":{
          |"id":{
            |"type":"string"
          |},
          |"name":{
            |"type":"string"
          |},
          |"price":{
            |"type":"number"
          |}
        |},
        |"required":["id","name","price"],
        |"additionalProperties":false
      |}""".stripMargin.replaceAll("[\n\r]","")

    SpecHelpers.HdfsTest.lookupSchema(schemaKey).map(_.map(_.toString)) must beSuccessful(Some(expected))
  }

  def e4 = {
    val schemaKey = SchemaKey("com.acme.n-a", "null", "jsonschema", "1-0-0")
    SpecHelpers.HdfsTest.lookupSchema(schemaKey) must beSuccessful(None)
  }

}
