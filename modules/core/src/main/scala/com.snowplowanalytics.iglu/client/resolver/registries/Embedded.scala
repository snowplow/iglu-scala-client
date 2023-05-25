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
package com.snowplowanalytics.iglu.client.resolver.registries

// Java
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}

import java.io.{File, InputStream}
import scala.util.matching.Regex

// Scala
import scala.io.Source
import scala.util.control.NonFatal

// Cats
import cats.effect.Sync
import cats.effect.implicits._
import cats.implicits._
import cats.data.EitherT

// circe
import io.circe.parser.parse
import io.circe.Json

import com.snowplowanalytics.iglu.core.SchemaList

private[registries] object Embedded {

  /**
   * Retrieves an Iglu Schema from the Embedded Iglu Repo as a JSON
   *
   * @param base path on the local filesystem system
   * @param key The SchemaKey uniquely identifying the schema in Iglu
   * @return either a `Json` on success, or `RegistryError` in case of any failure
   *         (i.e. all exceptions should be swallowed by `RegistryError`)
   */
  def lookup[F[_]: Sync](
    base: String,
    key: SchemaKey
  ): F[Either[RegistryError, Json]] = {
    val path   = RegistryLookup.toPath(base, key)
    val is     = readResource[F](path)
    val schema = is.bracket(_.traverse(fromStream[F]))(_.traverse_(closeStream[F]))
    val result = for {
      stringOption <- schema.attemptT.leftMap(Utils.repoFailure)
      string       <- EitherT.fromOption[F](stringOption, RegistryError.NotFound: RegistryError)
      json         <- EitherT.fromEither[F](parse(string)).leftMap(Utils.invalidSchema)
    } yield json

    result.value
  }

  /** Not-RT analog of [[Embedded.lookup]] */
  def unsafeLookup(path: String): Either[RegistryError, Json] =
    try {
      val is     = unsafeReadResource(path)
      val schema = is.map(unsafeFromStream)
      val result = schema
        .toRight(RegistryError.NotFound: RegistryError)
        .flatMap(x => parse(x).leftMap(Utils.invalidSchema))
      is.fold(())(unsafeCloseStream)
      result
    } catch {
      case NonFatal(e) =>
        e match {
          case _: NullPointerException => RegistryError.NotFound.asLeft
          case _                       => Utils.repoFailure(e).asLeft
        }
    }

  def unsafeList(path: String, modelMatch: Int): Either[RegistryError, SchemaList] =
    try {
      val d =
        new File(
          getClass.getResource(path).getPath
        ) // this will throw NPE for missing entry in embedded repos
      val schemaFileRegex: Regex = (".*/schemas/?" + // path to file
        "([a-zA-Z0-9-_.]+)/" +            // Vendor
        "([a-zA-Z0-9-_]+)/" +             // Name
        "([a-zA-Z0-9-_]+)/" +             // Format
        "([1-9][0-9]*)-(\\d+)-(\\d+)$").r // MODEL, REVISION and ADDITION

      def getFolderContent(d: File): List[String] = {
        d.listFiles
          .filter(_.isFile)
          .toList
          .filter(_.getName.startsWith(s"${modelMatch.toString}-"))
          .map(_.getAbsolutePath)
      }

      val content =
        if (d.exists & d.isDirectory)
          getFolderContent(d)
        else
          List.empty[String]

      content
        .traverse {
          case schemaFileRegex(vendor, name, format, model, revision, addition)
              if model == modelMatch.toString =>
            SchemaKey(
              vendor = vendor,
              name = name,
              format = format,
              version = SchemaVer
                .Full(model = model.toInt, revision = revision.toInt, addition = addition.toInt)
            ).asRight
          case f => RegistryError.RepoFailure(s"Corrupted schema file name at $f").asLeft
        }
        .map(_.sortBy(_.version))
        .flatMap(s =>
          if (s.isEmpty)
            RegistryError.NotFound.asLeft
          else
            s.asRight
        )
        .map(SchemaList.parseUnsafe)
    } catch {
      case NonFatal(e) =>
        e match {
          case _: NullPointerException => RegistryError.NotFound.asLeft
          case _                       => Utils.repoFailure(e).asLeft
        }
    }

  private def readResource[F[_]: Sync](path: String): F[Option[InputStream]] =
    Sync[F].delay(unsafeReadResource(path))
  private def unsafeReadResource(path: String): Option[InputStream] =
    Option(getClass.getResource(path)).map(_.openStream())

  private def fromStream[F[_]: Sync](is: InputStream): F[String] =
    Sync[F].delay(unsafeFromStream(is))
  private def unsafeFromStream(is: InputStream): String =
    Source.fromInputStream(is).mkString

  private def closeStream[F[_]: Sync](is: InputStream): F[Unit] =
    Sync[F].delay(unsafeCloseStream(is))
  private def unsafeCloseStream(is: InputStream): Unit =
    is.close()

}
