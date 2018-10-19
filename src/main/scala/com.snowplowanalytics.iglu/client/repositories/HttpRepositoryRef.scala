/*
 * Copyright (c) 2014-2018 Snowplow Analytics Ltd. All rights reserved.
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

// Java
import java.net.{URI, UnknownHostException}

// Scala
import scala.util.control.NonFatal

// Apache Commons
import org.apache.commons.lang3.exception.ExceptionUtils

// Cats
import cats.instances.either._
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.data.OptionT
import cats.effect.Sync

// circe
import io.circe._
import io.circe.Decoder.Result
import io.circe.optics.JsonPath._

// scalaj
import scalaj.http._

// Iglu Core
import com.snowplowanalytics.iglu.core.SchemaKey

// This project
import utils.{ValidationExceptions => VE}
import utils.SchemaKeyUtils
import validation.ProcessingMessage

/**
 * Helpers for constructing an HttpRepository.
 * See below for the definition.
 */
object HttpRepositoryRef {

  /**
   * Helper class to extract HTTP URI and api key from config JSON
   */
  private[client] case class HttpConnection(uri: String, apikey: Option[String])

  private implicit val httpConnectionDecoder: Decoder[HttpConnection] =
    new Decoder[HttpConnection] {
      def apply(c: HCursor): Result[HttpConnection] =
        for {
          uri    <- c.downField("uri").as[String]
          apikey <- c.downField("apikey").as[Option[String]]
        } yield HttpConnection(uri, apikey)
    }

  /**
   * Read a Json from an URI using optional apikey
   * with added optional header, so it is unsafe as well and throws same exceptions
   *
   * @param uri the URL to fetch the JSON document from
   * @param apikey optional apikey UUID to aunthenticate in Iglu HTTP repo
   * @return The document at that URL
   */
  private def getFromUri[F[_]: Sync](uri: URI, apikey: Option[String]): F[Option[String]] = {
    val request = apikey
      .map(key => Http(uri.toString).header("apikey", key))
      .getOrElse(Http(uri.toString))

    Sync[F]
      .delay(request.asString)
      .map(response => if (response.is2xx) response.body.some else None)
  }

  /**
   * Sniffs a config JSON to determine if this is
   * an HTTP-based repository ref or not.
   *
   * @param config The configuration JSON to sniff
   * @return true if this is the configuration for
   *         an HttpRepositoryRef, else false
   */
  def isHttp(config: Json): Boolean =
    extractUrl(config).isValid

  /**
   * Constructs an EmbeddedRepositoryRef
   * from a json.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this embedded
   *         repository
   */
  def parse(config: Json): ValidatedNelType[HttpRepositoryRef] = {
    val conf = RepositoryRefConfig.parse(config)
    val http = extractUrl(config)

    (conf, http.toValidatedNel).mapN { (c, h) =>
      HttpRepositoryRef(c, h.uri, h.apikey)
    }
  }

  /**
   * Returns the path to this embedded repository.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return the path to the embedded repository on
   *         Success, or an error String on Failure
   */
  private def extractUrl(config: Json): ValidatedType[HttpConnection] =
    root.connection.http.json
      .getOption(config)
      .flatMap(_.as[HttpConnection].toOption)
      .toValid(ProcessingMessage(s"Could not extract connection.http from ${config.spaces2}"))

  /**
   * A wrapper around Java's URI.
   *
   * @param url The String to
   *        convert to a URI
   * @return an URI, or an
   *         error message, all
   *         wrapped in an Either
   */
  private def stringToUri(url: String): Either[ProcessingMessage, URI] =
    try {
      URI.create(url).asRight
    } catch {
      case _: NullPointerException =>
        ProcessingMessage("Provided URL was null").asLeft
      case e: IllegalArgumentException =>
        val messageString = "Provided URI string [%s] violates RFC 2396: [%s]"
          .format(url, ExceptionUtils.getRootCause(e).getMessage)

        ProcessingMessage(messageString).asLeft
    }

}

/**
 * An HTTP repository is one which is accessible over
 * HTTP.
 */
case class HttpRepositoryRef(
  override val config: RepositoryRefConfig,
  uri: String,
  apikey: Option[String] = None)
    extends RepositoryRef {

  /**
   * De-prioritize searching this class of repository because
   * it is high cost.
   */
  override val classPriority: Int = 100

  /**
   * Human-readable descriptor for this
   * type of repository ref.
   */
  val descriptor = "HTTP"

  /**
   * Retrieves an IgluSchema from the Iglu Repo as
   * a Json.
   *
   * @param schemaKey The SchemaKey uniquely identifies
   *        the schema in Iglu
   * @return a Validation boxing either the Schema's
   *         Json on Success, or an error String
   *         on Failure
   */
  def lookupSchema[F[_]: Sync](schemaKey: SchemaKey): F[Either[ProcessingMessage, Option[Json]]] = {
    val uriNorm =  if (uri.endsWith("/")) uri.dropRight(1) else uri
    HttpRepositoryRef
      .stringToUri(SchemaKeyUtils.toPath(uriNorm, schemaKey))
      .traverse(uri => HttpRepositoryRef.getFromUri(uri, apikey))
      .map { jsonEither =>
        val result = for {
          jsonString <- OptionT(jsonEither)
          json <- OptionT.liftF(
            parser
              .parse(jsonString)
              .leftMap(failure => VE.parsingFailureToProcessingMessage(failure, schemaKey, config)))
        } yield json

        result.value
      }
      .handleError {
        case uhe: UnknownHostException =>
          ProcessingMessage(
            s"Unknown host issue fetching ${schemaKey} in ${descriptor} Iglu repository ${config.name}: ${uhe.getMessage}").asLeft
        case NonFatal(nfe) =>
          ProcessingMessage(
            s"Unexpected exception fetching $schemaKey in ${descriptor} Iglu repository ${config.name}: $nfe").asLeft
      }
  }
}
