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
    use exponential backoff between retries $usesExponentialBackoff
  """

  val testUri    = new URI("http://test.com/schema")
  val testApikey = Some("test-key")

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

      val result = JavaNetRegistryLookup.executeCallWithRetry(testUri, testApikey, 0, mockSend)

      (result must beSome("""{"test": "schema"}"""): MatchResult[Any]) and
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

      val result = JavaNetRegistryLookup.executeCallWithRetry(testUri, testApikey, 0, mockSend)

      (result must beNone: MatchResult[Any]) and
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

    val result = JavaNetRegistryLookup.executeCallWithRetry(testUri, testApikey, 0, mockSend)

    (result must beNone) and
      (attemptCounter.get() must beEqualTo(6)) // initial + 5 retries (MaxRetries = 5)
  }

  def usesExponentialBackoff: MatchResult[Any] = {
    val attemptCounter = new AtomicInteger(0)

    val mockSend: HttpRequest => HttpResponse[String] = { _ =>
      val attempt = attemptCounter.getAndIncrement()
      if (attempt < 3) createMockResponse(500, "Internal Server Error")
      else createMockResponse(200, """{"test": "schema"}""")
    }

    val startTime = System.currentTimeMillis()
    val result    = JavaNetRegistryLookup.executeCallWithRetry(testUri, testApikey, 0, mockSend)

    val totalTime = System.currentTimeMillis() - startTime
    // Expected backoffs: 100ms, 200ms, 400ms = 700ms minimum
    // Allow some tolerance for execution overhead
    (result must beSome("""{"test": "schema"}"""): MatchResult[Any]) and
      (attemptCounter.get() must beEqualTo(4)) and
      (totalTime must be_>(600L)) and
      (totalTime must be_<(800L))
  }
}
