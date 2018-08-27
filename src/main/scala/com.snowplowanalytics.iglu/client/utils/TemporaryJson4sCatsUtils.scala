package com.snowplowanalytics.iglu.client.utils

import cats._
import cats.implicits._
import cats.data.Validated.Valid

import cats.data.{Validated, ValidatedNel}
import org.json4s.JsonAST.{JNothing, JObject, JValue}
import org.json4s.{Formats, Reader}

// Just ignore this in the review, these functions are temporary (will be gone in the next PR)
object TemporaryJson4sCatsUtils {

  def validatedField[A: Manifest](name: String)(json: JValue)(
    implicit formats: Formats): ValidatedNel[Throwable, A] =
    json match {
      case JObject(fs) =>
        fs.find(_._1 == name)
          .map(f => f._2.extract[A].valid)
          .getOrElse(Validated.invalidNel(new RuntimeException(s"$name: $json")))
      case x =>
        Validated.invalidNel(new RuntimeException("failed"))
    }

  def validatedOptionalField[A: Manifest](name: String)(json: JValue)(
    implicit formats: Formats): ValidatedNel[Throwable, Option[A]] =
    json match {
      case JObject(fs) =>
        fs.find(_._1 == name)
          .map(f => f._2.extract[A].some.validNel[Throwable])
          .getOrElse(None.validNel[Throwable])
      case x =>
        Validated.invalidNel(new RuntimeException("failed"))
    }
}
