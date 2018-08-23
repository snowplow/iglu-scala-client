# Scala client for Iglu

[ ![Build Status] [travis-image] ] [travis] [ ![Release] [release-image] ] [releases] [ ![License] [license-image] ] [license]

A Scala client and resolver for **[Iglu schema repositories] [iglu-wiki]** from the team at **[Snowplow Analytics] [snowplow-website]**.

Iglu Scala Client is used extensively in **[Snowplow] [snowplow-repo]** to validate self-describing JSONs. For a presentation on how we came to build Iglu, see **[this blog post] [snowplow-schema-post]**.

![scala-client-img] [scala-client-img]

## Installation

The latest version of Iglu Scala Client is 0.5.0, which currently works with Scala 2.11.x

If you're using SBT, add the following lines to your build file:

```scala
val igluClient = "com.snowplowanalytics" %% "iglu-scala-client" % "0.5.0"
```

Note the double percent (`%%`) between the group and artifactId. That'll ensure you get the right package for your Scala version.

## Usage

Primary entity for working with Iglu Scala Client is `com.snowplowanalytics.iglu.client.Resolver`.
Resolver companion object has `parse` method which allows you to create Resolver instance from **[resolver configuration] [resolver-config]**.
Second working method is `lookupSchema`, receiving Schema key as String or directly `com.snowplowanalytics.iglu.SchemaKey` object,
this method traverse all configured repositories trying to find Schema by its key.

```scala
import cats.data.ValidatedNel
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jsonschema.core.report.ProcessingMessage
import com.snowplowanalytics.iglu.client.{ Resolver, SchemaKey }

val resolverConfig: JsonNode = ???
val schema: ValidatedNel[ProcessingMessage, JsonNode] = for {
  schemaKey <- SchemaKey.parseNel("iglu:com.snowplowanalytics.snowplow/mobile_context/jsonschema/1-0-0")
  resolver <- Resolver.parse(resolverConfig)
  schema <- resolver.lookupSchema(schemaKey)
} yield schema
```

Above snippet returns mobile context JSON Schema if you provide correct `resolverConfig`.
If not you will get all errors (like invalid format, network failure, etc) accumulated in `cats.data.NonEmptyList`,
which itself is left side of `ValidatedNel`, structure isomorphic to native Scala `Either`.

## Find out more

| **[Technical Docs] [techdocs]**     | **[Setup Guide] [setup]**     | **[Roadmap] [roadmap]**           | **[Contributing] [contributing]**           |
|-------------------------------------|-------------------------------|-----------------------------------|---------------------------------------------|
| [![i1] [techdocs-image]] [techdocs] | [![i2] [setup-image]] [setup] | [![i3] [roadmap-image]] [roadmap] | [![i4] [contributing-image]] [contributing] |

## Copyright and license

Iglu Scala Client is copyright 2014-2017 Snowplow Analytics Ltd.

Licensed under the **[Apache License, Version 2.0] [license]** (the "License");
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
[release-image]: http://img.shields.io/badge/release-0.5.0-blue.svg?style=flat

[license]: http://www.apache.org/licenses/LICENSE-2.0
[license-image]: http://img.shields.io/badge/license-Apache--2-blue.svg?style=flat
