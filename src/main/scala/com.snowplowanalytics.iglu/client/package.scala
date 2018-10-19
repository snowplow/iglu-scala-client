package com.snowplowanalytics.iglu

package object client {
  type Resolver[F[_]] = resolver.Resolver[F]
  type Validator[A]   = validator.Validator[A]

  val Resolver       = resolver.Resolver
  val CirceValidator = validator.CirceValidator
}
