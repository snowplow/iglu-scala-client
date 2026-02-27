package com.snowplowanalytics.iglu.jsonschema

import scala.collection.mutable

/** Lazily-initialized error accumulator. Zero VectorBuilder allocation on the happy path (valid documents). Only allocates the backing
  * VectorBuilder when the first error is actually appended.
  */
final class LazyErrors {
  private var b: mutable.Builder[ValidationError, Vector[ValidationError]] = _

  def +=(e: ValidationError): Unit = {
    if (b == null) b = Vector.newBuilder[ValidationError]
    val _ = b += e
  }

  def ++=(errs: Vector[ValidationError]): Unit = {
    if (errs.nonEmpty) {
      if (b == null) b = Vector.newBuilder[ValidationError]
      val _ = b ++= errs
    }
  }

  def result(): Vector[ValidationError] =
    if (b == null) Vector.empty else b.result()
}
