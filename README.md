# Scala client for Iglu

[![Build Status][travis-image]][travis]
[![Release][release-image]][releases]
[![License][license-image]][license]
[![Coverage Status][coveralls-image]][coveralls]

A Scala client and resolver for **[Iglu schema repositories][iglu-wiki]** from the team at **[Snowplow Analytics][snowplow-website]**.

Iglu Scala Client is used extensively in **[Snowplow][snowplow-repo]** to validate self-describing JSONs. For a presentation on how we came to build Iglu, see **[this blog post][snowplow-schema-post]**.

![scala-client-img][scala-client-img]

## Installation

The latest version of Iglu Scala Client is 0.6.2, which currently works with Scala 2.11 and 2.12.

If you're using SBT, add the following lines to your build file:

```scala
val igluClient = "com.snowplowanalytics" %% "iglu-scala-client" % "0.6.2"
```

Note the double percent (`%%`) between the group and artifactId. That'll ensure you get the right package for your Scala version.

## API

Iglu Scala Client has "tagless final"-friendly API, allowing end-users to abstract over effects they use:

* `cats.effect.IO`, ZIO or similar lazy referentially-transparent implementation for most production use cases
* `cats.data.State` for testing purposes
* `Id` when you need an eager implementation

Generally, we highly recommend to use a referentially-transparent implementation,
but in some environments like Apache Spark or Apache Beam it is not possible to use lazy implementation.
In Spark and similar environments we recommend to use `Id`, but everything needs to be tested in real environment.

Primary entity for working with Iglu Scala Client is `com.snowplowanalytics.iglu.client.Client`.
It consists of two objects: `Resolver` and `Validator`. First one responsible for registry traversal, schema resolution, fetching and caching.
While second one just checks the datum against resolved schema and returns a report.

`Client` has only one method:

```
def check[F[_]: Monad: Clock: RegistryLookup](instance: SelfDescribingData[Json]): EitherT[F, ClientError, Unit]
```

This method performs resolution and validation steps altogether. Also you can use `Resolver` and `Validator` separately.

* `F[_]` is an effect type, usually representing a side-effect
* `Monad` is an implicit evidence that `F` has an instance of `cats.Monad` type class
* `Clock` is an implicit evidence that `F` has an instance of `cats.effect.Clock` capability and thus has an access to wallclock
* `RegistryLookup` is an implicit evidence that `F` can perform lookups. Its a tagless-final capability, defined for Iglu Client.
* `ClientError` is an error ADT, representing a failure happened either due resolution or validation step

`Client` companion object also has `parseDefault` method which allows you to instantiate `Client` instance from **[resolver configuration][resolver-config]**. It will also instantiate a mutable cache instance (thus, result is wrapped in `F[_]`).

## Example

Second working method is `lookupSchema`, receiving Schema key as String or directly `com.snowplowanalytics.iglu.SchemaKey` object,
this method traverse all configured repositories trying to find Schema by its key.

```scala
import io.circe.Json
import io.circe.literal._   // circe-literal is not bundled with Iglu Client

import cats.effect.IO
import cats.syntax.show._

import com.snowplowanalytics.iglu.client.Client
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer, SelfDescribingData}

implicit val clockIoInstance: Clock[IO] = Clock.create[IO] // Usually provided by IOApp

val resolverConfig: Json = json"""{
  "schema": "iglu:com.snowplowanalytics.iglu/resolver-config/jsonschema/1-0-1",
  "data": {
    "cacheSize": 5,
    "repositories": [{
      "name": "Iglu Central",
      "priority": 0,
      "vendorPrefixes": [ "com.snowplowanalytics" ],
      "connection": { "http": { "uri": "http://iglucentral.com" } }
    }]
  }
}"""

val instance = SelfDescribingData(
  SchemaKey("com.snowplowanalytics.snowplow", "geolocation_context", "jsonschema", SchemaVer.Full(1,1,0)),
  json"""{"latitude": 3, "longitude": 0}""")

val result = for {
  client <- Client.parseDefault[IO](resolverConfig).leftMap(_.show) // It can be a DecodingError
  _ <- client.check(instance).leftMap(_.show) // ClientError has an instance of Show type class
} yield ()
```

Above snippet will return a lazy side-effect that can be either a ClientError in case data is invalid against its schema
or there some kind of resolution problems, or simply nothing (`Unit`) in case of success

## Find out more

| **[Technical Docs][techdocs]**     | **[Setup Guide][setup]**     | **[Roadmap][roadmap]**           | **[Contributing][contributing]**           |
|-------------------------------------|-------------------------------|-----------------------------------|---------------------------------------------|
| [![i1][techdocs-image]][techdocs] | [![i2][setup-image]][setup] | [![i3][roadmap-image]][roadmap] | [![i4][contributing-image]][contributing] |

## Copyright and license

Iglu Scala Client is copyright 2014-2020 Snowplow Analytics Ltd.

Licensed under the **[Apache License, Version 2.0][license]** (the "License");
you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

[scala-client-img]: https://github.com/snowplow/iglu/wiki/technical-documentation/images/iglu-clients.png

[iglu-wiki]: https://github.com/snowplow/iglu/wiki
[snowplow-schema-post]: http://snowplowanalytics.com/blog/2014/06/06/making-snowplow-schemas-flexible-a-technical-approach/
[resolver-config]: https://github.com/snowplow/iglu/wiki/Iglu-client-configuration

[snowplow-repo]: https://github.com/snowplow/snowplow
[snowplow-website]: http://snowplowanalytics.com

[techdocs-image]: https://d3i6fms1cm1j0i.cloudfront.net/github/images/techdocs.png
[setup-image]: https://d3i6fms1cm1j0i.cloudfront.net/github/images/setup.png
[roadmap-image]: https://d3i6fms1cm1j0i.cloudfront.net/github/images/roadmap.png
[contributing-image]: https://d3i6fms1cm1j0i.cloudfront.net/github/images/contributing.png

[techdocs]: https://github.com/snowplow/iglu/wiki/Scala-client
[setup]: https://github.com/snowplow/iglu/wiki/Scala-client-setup
[roadmap]: https://github.com/snowplow/iglu/wiki/Product-roadmap
[contributing]: https://github.com/snowplow/iglu/wiki/Contributing

[travis]: https://travis-ci.org/snowplow/iglu-scala-client
[travis-image]: https://travis-ci.org/snowplow/iglu-scala-client.png?branch=master

[releases]: https://github.com/snowplow/iglu-scala-client/releases
[release-image]: https://maven-badges.herokuapp.com/maven-central/com.snowplowanalytics/iglu-scala-client_2.12/badge.svg

[license]: http://www.apache.org/licenses/LICENSE-2.0
[license-image]: http://img.shields.io/badge/license-Apache--2-blue.svg?style=flat

[coveralls]: https://coveralls.io/github/snowplow/iglu-scala-client?branch=master
[coveralls-image]: https://coveralls.io/repos/github/snowplow/iglu-scala-client/badge.svg?branch=master
