package io.circe

/**
 * Bridge for zero-allocation access to JsonObject internals.
 *
 * Placed in `io.circe` to access `private[circe]` members such as `applyUnsafe` and `JString`.
 */
object JsonObjectUnsafe {

  /**
   * Zero-allocation field access. `applyUnsafe` is `private[circe]` and bypasses `Option` wrapping. Only safe when the key is known to
   * exist (e.g. from iterating `obj.keys`).
   */
  def getValue(obj: JsonObject, key: String): Json =
    obj.applyUnsafe(key)

  /**
   * Returns the raw String value if json is a JString, or null for any other Json type.
   *
   * Avoids the `Option[String]` boxing of `json.asString`. `JString` is `private[circe]` and is accessible here because we are in the
   * `io.circe` package. The null sentinel lets call sites use a simple null-check rather than pattern matching.
   */
  def stringValue(json: Json): String = json match {
    case Json.JString(s) => s
    case _               => null
  }
}
