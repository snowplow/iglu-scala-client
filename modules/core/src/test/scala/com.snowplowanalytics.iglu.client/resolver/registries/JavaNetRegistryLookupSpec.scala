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

import org.specs2.Specification
import org.specs2.matcher.MatchResult

import java.net.URI
import java.net.http.{HttpRequest, HttpResponse}
import java.util.concurrent.atomic.AtomicInteger

class JavaNetRegistryLookupSpec extends Specification {
  def is = s2"""
  executeCallWithRetry should
    retry on retriable status codes (500, 503, 429, 408) and eventually succeed $retriesOnRetriableStatusCodesAndSucceeds
    not retry on non-retriable status codes (404, 400) $doesNotRetryOnNonRetriableStatusCodes
    exhaust retries after max attempts on persistent 500 errors $exhaustsRetriesAfterMaxAttempts
    return NotFound on 403 and 404 without retrying $notFoundOn403And404
    return ClientFailure on other 4xx without retrying $clientFailureOnOther4xx
    return ClientFailure on 408 and 429 after retries are exhausted $clientFailureOn408And429
    return RepoFailure on 5xx after retries are exhausted $repoFailureOn5xx
    return a non-NotFound error on a persistent exception after retries are exhausted $errorOnPersistentException
    use exponential backoff between retries $usesExponentialBackoff
  """

  val testUri    = new URI("http://test.com/schema")
  val testApikey = Some("test-key")

  // Retry tests never sleep for real
  val noSleep: Long => Unit = _ => ()

  // Helper to create a mock HttpResponse with a status code
  def createMockResponse(statusCodeParam: Int, bodyParam: String = ""): HttpResponse[String] =
    new HttpResponse[String] {
      def statusCode(): Int                                            = statusCodeParam
      def body(): String                                               = bodyParam
      def headers(): java.net.http.HttpHeaders                         = null
      def request(): HttpRequest                                       = null
      def previousResponse(): java.util.Optional[HttpResponse[String]] = java.util.Optional.empty()
      def uri(): URI                                                   = testUri
      def version(): java.net.http.HttpClient.Version                  = null
      def sslSession(): java.util.Optional[javax.net.ssl.SSLSession]   = java.util.Optional.empty()
    }

  def retriesOnRetriableStatusCodesAndSucceeds: MatchResult[Any] = {
    // Test all retriable status codes: 500, 503, 429, 408
    val retriableStatusCodes = List(500, 503, 429, 408)

    val results = retriableStatusCodes.map { statusCode =>
      val attemptCounter = new AtomicInteger(0)
      val mockSend: HttpRequest => HttpResponse[String] = { _ =>
        val attempt = attemptCounter.getAndIncrement()
        if (attempt < 1) createMockResponse(statusCode, s"Error $statusCode")
        else createMockResponse(200, """{"test": "schema"}""")
      }

      val result =
        JavaNetRegistryLookup.executeCallWithRetry(testUri, testApikey, 0, mockSend, noSleep)

      (result must beRight("""{"test": "schema"}"""): MatchResult[Any]) and
        (attemptCounter.get() must beEqualTo(2)) // initial + 1 retry
    }

    results.reduce(_ and _)
  }

  def doesNotRetryOnNonRetriableStatusCodes: MatchResult[Any] = {
    // Test all non-retriable status codes: 404, 400
    val nonRetriableStatusCodes = List(404, 400)

    val results = nonRetriableStatusCodes.map { statusCode =>
      val attemptCounter = new AtomicInteger(0)
      val mockSend: HttpRequest => HttpResponse[String] = { _ =>
        attemptCounter.incrementAndGet()
        createMockResponse(statusCode, s"Error $statusCode")
      }

      val result =
        JavaNetRegistryLookup.executeCallWithRetry(testUri, testApikey, 0, mockSend, noSleep)

      (result must beLeft: MatchResult[Any]) and
        (attemptCounter.get() must beEqualTo(1)) // no retries
    }

    results.reduce(_ and _)
  }

  def exhaustsRetriesAfterMaxAttempts: MatchResult[Any] = {
    val attemptCounter = new AtomicInteger(0)
    val mockSend: HttpRequest => HttpResponse[String] = { _ =>
      attemptCounter.incrementAndGet()
      createMockResponse(500, "Internal Server Error")
    }

    val result =
      JavaNetRegistryLookup.executeCallWithRetry(testUri, testApikey, 0, mockSend, noSleep)

    (result must beLeft(beAnInstanceOf[RegistryError.RepoFailure])) and
      (attemptCounter.get() must beEqualTo(6)) // initial + 5 retries (MaxRetries = 5)
  }

  def usesExponentialBackoff: MatchResult[Any] = {
    val attemptCounter = new AtomicInteger(0)

    val mockSend: HttpRequest => HttpResponse[String] = { _ =>
      val attempt = attemptCounter.getAndIncrement()
      if (attempt < 3) createMockResponse(500, "Internal Server Error")
      else createMockResponse(200, """{"test": "schema"}""")
    }

    val delays                    = scala.collection.mutable.ListBuffer.empty[Long]
    val recordSleep: Long => Unit = delays += _
    val result =
      JavaNetRegistryLookup.executeCallWithRetry(testUri, testApikey, 0, mockSend, recordSleep)

    (result must beRight("""{"test": "schema"}"""): MatchResult[Any]) and
      (attemptCounter.get() must beEqualTo(4)) and
      (delays.toList must beEqualTo(List(100L, 200L, 400L)))
  }

  // Returns the result and the number of attempts made
  def runWithStatus(statusCode: Int): (Either[RegistryError, String], Int) = {
    val attemptCounter = new AtomicInteger(0)
    val mockSend: HttpRequest => HttpResponse[String] = { _ =>
      attemptCounter.incrementAndGet()
      createMockResponse(statusCode, s"Body of $statusCode")
    }
    val result =
      JavaNetRegistryLookup.executeCallWithRetry(testUri, testApikey, 0, mockSend, noSleep)
    (result, attemptCounter.get())
  }

  def notFoundOn403And404: MatchResult[Any] =
    List(403, 404)
      .map { statusCode =>
        val (result, attempts) = runWithStatus(statusCode)
        (result must beLeft(RegistryError.NotFound: RegistryError): MatchResult[Any]) and
          (attempts must beEqualTo(1))
      }
      .reduce(_ and _)

  def clientFailureOnOther4xx: MatchResult[Any] =
    List(400, 401, 405, 410)
      .map { statusCode =>
        val (result, attempts) = runWithStatus(statusCode)
        (result must beLeft(
          RegistryError.ClientFailure(s"Unexpected response code: $statusCode"): RegistryError
        ): MatchResult[Any]) and
          (attempts must beEqualTo(1))
      }
      .reduce(_ and _)

  def clientFailureOn408And429: MatchResult[Any] =
    List(408, 429)
      .map { statusCode =>
        val (result, attempts) = runWithStatus(statusCode)
        (result must beLeft(
          RegistryError.ClientFailure(s"Unexpected response code: $statusCode"): RegistryError
        ): MatchResult[Any]) and
          (attempts must beEqualTo(6))
      }
      .reduce(_ and _)

  def repoFailureOn5xx: MatchResult[Any] =
    List(500, 503)
      .map { statusCode =>
        val (result, attempts) = runWithStatus(statusCode)
        (result must beLeft(
          RegistryError.RepoFailure(s"Unexpected server response code: $statusCode"): RegistryError
        ): MatchResult[Any]) and
          (attempts must beEqualTo(6))
      }
      .reduce(_ and _)

  def errorOnPersistentException: MatchResult[Any] = {
    val attemptCounter = new AtomicInteger(0)
    val mockSend: HttpRequest => HttpResponse[String] = { _ =>
      attemptCounter.incrementAndGet()
      throw new java.io.IOException("Connection refused")
    }

    val result =
      JavaNetRegistryLookup.executeCallWithRetry(testUri, testApikey, 0, mockSend, noSleep)

    (result must beLeft(RegistryError.RepoFailure("Connection refused"): RegistryError)) and
      (result must not(beLeft(RegistryError.NotFound: RegistryError))) and
      (attemptCounter.get() must beEqualTo(6))
  }

}
