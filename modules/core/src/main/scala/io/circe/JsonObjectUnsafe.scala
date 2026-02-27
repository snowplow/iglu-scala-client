package io.circe

/** Bridge for zero-allocation access to JsonObject internals.
  *
  * Placed in `io.circe` to access `private[circe]` members such as `applyUnsafe` and `JString`.
  */
object JsonObjectUnsafe {

  /** Zero-allocation field access. `applyUnsafe` is `private[circe]` and bypasses `Option` wrapping. Only safe when the key is known to
    * exist (e.g. from iterating `obj.keys`).
    */
  def getValue(obj: JsonObject, key: String): Json =
    obj.applyUnsafe(key)

  /** Returns the raw String value if json is a JString, or null for any other Json type.
    *
    * Avoids the `Option[String]` boxing of `json.asString`. `JString` is `private[circe]` and is accessible here because we are in the
    * `io.circe` package. The null sentinel lets call sites use a simple null-check rather than pattern matching.
    */
  def stringValue(json: Json): String = json match {
    case Json.JString(s) => s
    case _               => null
  }

  /** Returns the JsonNumber if json is a JNumber, or null for any other Json type.
    *
    * Avoids `Option[JsonNumber]` boxing of `json.asNumber`.
    */
  def numberValue(json: Json): JsonNumber = json match {
    case Json.JNumber(n) => n
    case _               => null
  }

  /** Returns the Vector[Json] if json is a JArray, or null for any other Json type.
    *
    * Avoids `Option[Vector[Json]]` boxing of `json.asArray`.
    */
  def arrayValue(json: Json): Vector[Json] = json match {
    case Json.JArray(a) => a
    case _              => null
  }

  /** Returns the JsonObject if json is a JObject, or null for any other Json type.
    *
    * Avoids `Option[JsonObject]` boxing of `json.asObject`.
    */
  def objectValue(json: Json): JsonObject = json match {
    case Json.JObject(o) => o
    case _               => null
  }
}
