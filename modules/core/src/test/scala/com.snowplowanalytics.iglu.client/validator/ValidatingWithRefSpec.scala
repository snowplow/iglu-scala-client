package com.snowplowanalytics.iglu.client.validator

import io.circe.literal.JsonStringContext
import org.specs2.mutable.Specification

class ValidatingWithRefSpec extends Specification {

  val schema =
    json"""
         {
           "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
           "description": "Test schema using $$ref with 'http'/'https' protocols",
           "self": {
             "vendor": "com.test",
             "name": "test",
             "format": "jsonschema",
             "version": "1-0-0"
           },
           "properties": {
             "id": {
               "allOf": [
                 {"$$ref": "http://json-schema.org/draft-04/schema#"},
                 {"$$ref": "https://json-schema.org/draft-04/schema"},
                 {"$$ref": "http://anything"},
                 {"$$ref": "https://anything"}
               ]
             }
           }
         } 
      """

  "Validator should ignore '$ref' keyword" in {
    val data = json"""{"id": "can_be_anything1234"}"""
    CirceValidator.validate(data, schema) must beRight(())
  }
}
