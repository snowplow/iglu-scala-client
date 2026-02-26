/*
 * Copyright (c) 2014-present Snowplow Analytics Ltd. All rights reserved.
 *
 * This software is made available by Snowplow Analytics, Ltd.,
 * under the terms of the Snowplow Limited Use License Agreement, Version 1.1
 * located at https://docs.snowplow.io/limited-use-license-1.1
 * BY INSTALLING, DOWNLOADING, ACCESSING, USING OR DISTRIBUTING ANY PORTION
 * OF THE SOFTWARE, YOU AGREE TO THE TERMS OF SUCH LICENSE AGREEMENT.
 */
package com.snowplowanalytics.iglu.jsonschema.formats

import java.net.URI
import java.time.LocalDate
import java.util.regex.Pattern
import scala.util.Try

/**
 * Format validator for the `format` keyword.
 */
trait Format {
  def name: String
  def validate(value: String): Boolean
}

object Formats {

  // IPv4: no leading zeroes allowed (octets must be 0 or start with 1-9)
  private val ipv4Pattern = Pattern.compile(
    "^((25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])\\.){3}" +
      "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])$"
  )

  // IPv4 decimal octet pattern for embedding in IPv6
  private val ipv4DecOctet = "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])"
  private val ipv4Embedded = s"($ipv4DecOctet\\.){3}$ipv4DecOctet"

  // IPv6 - comprehensive pattern without zone IDs
  // Supports pure IPv6 and IPv6 with embedded IPv4 (mixed format)
  private val ipv6Pattern = Pattern.compile(
    "^(" +
      // Pure IPv6 forms
      "([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}|" +           // Full form: 8 groups
      "([0-9a-fA-F]{1,4}:){1,7}:|" +                        // With trailing ::
      "([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|" +        // 7 groups with ::
      "([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|" + // 6 groups with ::
      "([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|" + // 5 groups with ::
      "([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|" + // 4 groups with ::
      "([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|" + // 3 groups with ::
      "[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|" +      // 2 groups with ::
      ":((:[0-9a-fA-F]{1,4}){1,7}|:)|" +                    // :: at start
      // Mixed IPv6/IPv4 forms (IPv4 takes 2 groups worth)
      s"::([fF]{4}(:0{1,4})?:)?$ipv4Embedded|" +   // ::ffff:IPv4 or ::IPv4
      s"([0-9a-fA-F]{1,4}:){1,4}:$ipv4Embedded|" + // n groups :: IPv4
      s"([0-9a-fA-F]{1,4}:){6}$ipv4Embedded|" +    // 6 groups:IPv4 (no ::)
      s"([0-9a-fA-F]{1,4}:){1,5}:[0-9a-fA-F]{1,4}:$ipv4Embedded|" + // With :: and one group before IPv4
      s"([0-9a-fA-F]{1,4}:){1}:([0-9a-fA-F]{1,4}:){0,4}$ipv4Embedded" + // 1:: with groups then IPv4
      ")$"
  )

  // Email: RFC 5321 - no dots at start/end, no consecutive dots
  private val emailPattern = Pattern.compile(
    "^[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+" +
      "(?:\\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)*" +
      "@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?" +
      "(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
  )

  private val hostnamePattern = Pattern.compile(
    "^[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(\\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
  )

  private val uuidPattern = Pattern.compile(
    "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
  )

  private val datePattern = Pattern.compile(
    "^\\d{4}-\\d{2}-\\d{2}$"
  )

  val ipv4: Format = new Format {
    val name                             = "ipv4"
    def validate(value: String): Boolean = ipv4Pattern.matcher(value).matches()
  }

  val ipv6: Format = new Format {
    val name = "ipv6"
    def validate(value: String): Boolean = {
      // Reject zone IDs (%) as they're not part of standard IPv6
      if (value.contains("%")) return false
      ipv6Pattern.matcher(value).matches()
    }
  }

  val email: Format = new Format {
    val name                             = "email"
    def validate(value: String): Boolean = emailPattern.matcher(value).matches()
  }

  val hostname: Format = new Format {
    val name = "hostname"
    def validate(value: String): Boolean =
      value.length <= 253 && hostnamePattern.matcher(value).matches()
  }

  val uri: Format = new Format {
    val name = "uri"
    def validate(value: String): Boolean = {
      // Check for invalid characters that Java's URI parser might accept
      // RFC 3986: unreserved / pct-encoded / sub-delims / ":" / "@" / "/" / "?"
      // Non-ASCII characters must be percent-encoded
      if (value.exists(c => c > 127)) return false

      Try {
        val u = new URI(value)
        // Must have a scheme and the URI must be absolute
        u.getScheme != null && u.isAbsolute
      }.getOrElse(false)
    }
  }

  val uuid: Format = new Format {
    val name                             = "uuid"
    def validate(value: String): Boolean = uuidPattern.matcher(value).matches()
  }

  // RFC 3339 date-time pattern with offset capture (no extended year format allowed)
  private val dateTimePattern = Pattern.compile(
    "^(\\d{4})-(\\d{2})-(\\d{2})[Tt](\\d{2}):(\\d{2}):(\\d{2})(\\.\\d+)?([Zz]|([+-])(\\d{2}):(\\d{2}))$"
  )

  val dateTime: Format = new Format {
    val name = "date-time"
    def validate(value: String): Boolean = {
      // Reject extended year format (+ prefix not allowed in RFC 3339)
      if (value.startsWith("+") || value.startsWith("-")) return false

      val m = dateTimePattern.matcher(value)
      if (!m.matches()) return false

      try {
        val year      = m.group(1).toInt
        val month     = m.group(2).toInt
        val day       = m.group(3).toInt
        val hour      = m.group(4).toInt
        val minute    = m.group(5).toInt
        val second    = m.group(6).toInt
        val offsetStr = m.group(8)

        // Basic range checks
        if (month < 1 || month > 12) return false
        if (day < 1 || day > 31) return false
        if (hour < 0 || hour > 23) return false
        if (minute < 0 || minute > 59) return false

        // Validate offset range (must be between -23:59 and +23:59)
        if (offsetStr != null && !offsetStr.equalsIgnoreCase("Z")) {
          val offsetHour = m.group(10).toInt
          val offsetMin  = m.group(11).toInt
          if (offsetHour > 23 || offsetMin > 59) return false
        }

        // Handle leap seconds: only valid at 23:59:60 UTC
        if (second == 60) {
          if (minute != 59) return false
          // Leap second must be at UTC 23:59:60
          // Convert local time + offset to UTC and check if it's 23:59
          val utcHour = if (offsetStr == null || offsetStr.equalsIgnoreCase("Z")) {
            hour
          } else {
            val sign        = if (m.group(9) == "+") 1 else -1
            val offsetHours = m.group(10).toInt
            // UTC hour = local hour - offset (+ means ahead of UTC)
            (hour - sign * offsetHours + 24) % 24
          }
          if (utcHour != 23) return false
          true
        } else if (second < 0 || second > 59) {
          false
        } else {
          // Validate the date is real
          Try {
            LocalDate.of(year, month, day)
            true
          }.getOrElse(false)
        }
      } catch {
        case _: Exception => false
      }
    }
  }

  val date: Format = new Format {
    val name = "date"
    def validate(value: String): Boolean = {
      if (!datePattern.matcher(value).matches()) return false
      Try {
        LocalDate.parse(value)
        true
      }.getOrElse(false)
    }
  }

  /** All supported formats */
  val all: Map[String, Format] = Map(
    "ipv4"      -> ipv4,
    "ipv6"      -> ipv6,
    "email"     -> email,
    "hostname"  -> hostname,
    "uri"       -> uri,
    "uuid"      -> uuid,
    "date-time" -> dateTime,
    "date"      -> date
  )

  def get(name: String): Option[Format] = all.get(name)
}

/**
 * Pattern strings for format error messages. Uses networknt-compatible patterns for test compatibility.
 */
object FormatPatterns {
  // IPv4 pattern in networknt format
  private val Ipv4Pattern =
    "^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])$"

  private val patterns: Map[String, String] = Map(
    "ipv4"      -> Ipv4Pattern,
    "ipv6"      -> "(ipv6)",
    "email"     -> "(email)",
    "hostname"  -> "(hostname)",
    "uri"       -> "(uri)",
    "uuid"      -> "(uuid)",
    "date-time" -> "(date-time)",
    "date"      -> "(date)"
  )

  def get(name: String): String = patterns.getOrElse(name, s"($name)")
}
