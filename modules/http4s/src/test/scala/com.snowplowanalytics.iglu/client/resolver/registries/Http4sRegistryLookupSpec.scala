/*
 * Copyright (c) 2023 Snowplow Analytics Ltd. All rights reserved.
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
package com.snowplowanalytics.iglu.client.resolver.registries

import cats.effect.testing.specs2.CatsEffect
import cats.effect.{IO, Resource}
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryError.{
  ClientFailure,
  RepoFailure
}
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList, SchemaVer}
import io.circe.Json
import org.http4s.client.{Client => HttpClient}
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.specs2.mutable.Specification

import java.net.URI

class Http4sRegistryLookupSpec extends Specification with CatsEffect {

  "The Http4sRegistryLookup lookup" should {
    "return schema on 200" in {

      val repositoryRef =
        Registry.Http(
          Registry.Config("name", 1, Nil),
          Registry.HttpConnection(URI.create("http://custom-iglu.com"), None)
        )
      val schemaKey = SchemaKey("com.myvendor", "myname", "jsonschema", SchemaVer.Full(42, 42, 42))

      val client = HttpClient[IO] { req =>
        val dsl = new Http4sDsl[IO] {}; import dsl._
        if (req.uri == uri"http://custom-iglu.com/schemas/com.myvendor/myname/jsonschema/42-42-42")
          Resource.eval(Ok("""{"xyz": true}"""))
        else
          Resource.eval(NotFound("unexpected url"))
      }

      Http4sRegistryLookup(client).lookup(repositoryRef, schemaKey).map { result =>
        result should beRight(Json.obj("xyz" -> Json.True))
      }
    }

    "return NotFound on 404" in {

      val repositoryRef =
        Registry.Http(
          Registry.Config("name", 1, Nil),
          Registry.HttpConnection(URI.create("http://custom-iglu.com"), None)
        )
      val schemaKey = SchemaKey("com.myvendor", "myname", "jsonschema", SchemaVer.Full(42, 42, 42))

      val client = HttpClient[IO] { _ =>
        val dsl = new Http4sDsl[IO] {}; import dsl._
        Resource.eval(NotFound("not found"))
      }

      Http4sRegistryLookup(client).lookup(repositoryRef, schemaKey).map { result =>
        result should beLeft(RegistryError.NotFound: RegistryError)
      }
    }

    "return ClientFailure on client exception" in {

      val repositoryRef =
        Registry.Http(
          Registry.Config("name", 1, Nil),
          Registry.HttpConnection(URI.create("http://custom-iglu.com"), None)
        )
      val schemaKey = SchemaKey("com.myvendor", "myname", "jsonschema", SchemaVer.Full(42, 42, 42))

      val client = HttpClient[IO] { _ =>
        Resource.eval(IO.raiseError(new RuntimeException("boom!")))
      }

      Http4sRegistryLookup(client).lookup(repositoryRef, schemaKey).map { result =>
        result should beLeft(ClientFailure("boom!"))
      }
    }

    "return NotFound on 403" in {

      val repositoryRef =
        Registry.Http(
          Registry.Config("name", 1, Nil),
          Registry.HttpConnection(URI.create("http://custom-iglu.com"), None)
        )
      val schemaKey = SchemaKey("com.myvendor", "myname", "jsonschema", SchemaVer.Full(42, 42, 42))

      val client = HttpClient[IO] { _ =>
        val dsl = new Http4sDsl[IO] {}; import dsl._
        Resource.eval(Forbidden("forbidden"))
      }

      Http4sRegistryLookup(client).lookup(repositoryRef, schemaKey).map { result =>
        result should beLeft(RegistryError.NotFound: RegistryError)
      }
    }

    "return ClientFailure on 408" in {

      val repositoryRef =
        Registry.Http(
          Registry.Config("name", 1, Nil),
          Registry.HttpConnection(URI.create("http://custom-iglu.com"), None)
        )
      val schemaKey = SchemaKey("com.myvendor", "myname", "jsonschema", SchemaVer.Full(42, 42, 42))

      val client = HttpClient[IO] { _ =>
        val dsl = new Http4sDsl[IO] {}; import dsl._
        Resource.eval(RequestTimeout("timeout"))
      }

      Http4sRegistryLookup(client).lookup(repositoryRef, schemaKey).map { result =>
        result should beLeft(ClientFailure("Unexpected response code: 408"))
      }
    }

    "return RepoFailure on 500" in {

      val repositoryRef =
        Registry.Http(
          Registry.Config("name", 1, Nil),
          Registry.HttpConnection(URI.create("http://custom-iglu.com"), None)
        )
      val schemaKey = SchemaKey("com.myvendor", "myname", "jsonschema", SchemaVer.Full(42, 42, 42))

      val client = HttpClient[IO] { _ =>
        val dsl = new Http4sDsl[IO] {}; import dsl._
        Resource.eval(InternalServerError("internal error"))
      }

      Http4sRegistryLookup(client).lookup(repositoryRef, schemaKey).map { result =>
        result should beLeft(RepoFailure("Unexpected server response: internal error"))
      }
    }

    "return ClientFailure on 200 with invalid JSON" in {

      val repositoryRef =
        Registry.Http(
          Registry.Config("name", 1, Nil),
          Registry.HttpConnection(URI.create("http://custom-iglu.com"), None)
        )
      val schemaKey = SchemaKey("com.myvendor", "myname", "jsonschema", SchemaVer.Full(42, 42, 42))

      val client = HttpClient[IO] { _ =>
        val dsl = new Http4sDsl[IO] {}; import dsl._
        Resource.eval(Ok("not valid json"))
      }

      Http4sRegistryLookup(client).lookup(repositoryRef, schemaKey).map { result =>
        result should beLeft.like { case ClientFailure(msg) =>
          msg must startWith("Could not decode server response.")
        }
      }
    }
  }

  "The Http4sRegistryLookup list" should {
    "return schema list on 200" in {

      val registry =
        Registry.Http(
          Registry.Config("name", 1, Nil),
          Registry.HttpConnection(URI.create("http://custom-iglu.com"), None)
        )

      val client = HttpClient[IO] { req =>
        val dsl = new Http4sDsl[IO] {}; import dsl._
        if (req.uri == uri"http://custom-iglu.com/schemas/com.myvendor/myname/jsonschema/42")
          Resource.eval(
            Ok(
              """["iglu:com.myvendor/myname/jsonschema/42-0-0", "iglu:com.myvendor/myname/jsonschema/42-0-1"]"""
            )
          )
        else
          Resource.eval(NotFound("unexpected url"))
      }

      val expected = List(
        SchemaKey("com.myvendor", "myname", "jsonschema", SchemaVer.Full(42, 0, 0)),
        SchemaKey("com.myvendor", "myname", "jsonschema", SchemaVer.Full(42, 0, 1))
      )

      Http4sRegistryLookup(client).list(registry, "com.myvendor", "myname", 42).map { result =>
        result should beRight(SchemaList.parseUnsafe(expected))
      }
    }

    "return ClientFailure on client exception" in {

      val registry =
        Registry.Http(
          Registry.Config("name", 1, Nil),
          Registry.HttpConnection(URI.create("http://custom-iglu.com"), None)
        )

      val client = HttpClient[IO] { _ =>
        Resource.eval(IO.raiseError(new RuntimeException("boom!")))
      }

      Http4sRegistryLookup(client).list(registry, "com.myvendor", "myname", 42).map { result =>
        result should beLeft(ClientFailure("boom!"))
      }
    }

  }

}
