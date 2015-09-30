# Scala client for Iglu [![Build Status](https://travis-ci.org/snowplow/iglu-scala-client.png?branch=master)](https://travis-ci.org/snowplow/iglu-scala-client)

A Scala client and resolver for **[Iglu schema repositories] [iglu-wiki]** from the team at **[Snowplow Analytics] [snowplow-website]**.

Iglu Scala Client is used extensively in **[Snowplow] [snowplow-repo]** to validate self-describing JSONs. For a presentation on how we came to build Iglu, see **[this blog post] [snowplow-schema-post]**.

![scala-client-img] [scala-client-img]

## Developer quickstart

Assuming git, **[Vagrant] [vagrant-install]** and **[VirtualBox] [virtualbox-install]** installed:

```bash
 host> git clone https://github.com/snowplow/iglu-scala-client
 host> cd iglu-scala-client
 host> vagrant up && vagrant ssh
guest> cd /vagrant
guest> sbt compile
```

## Find out more

| **[Technical Docs] [techdocs]**     | **[Setup Guide] [setup]**     | **[Roadmap] [roadmap]**           | **[Contributing] [contributing]**           |
|-------------------------------------|-------------------------------|-----------------------------------|---------------------------------------------|
| [![i1] [techdocs-image]] [techdocs] | [![i2] [setup-image]] [setup] | [![i3] [roadmap-image]] [roadmap] | [![i4] [contributing-image]] [contributing] |

## Copyright and license

Iglu Scala Client is copyright 2014 Snowplow Analytics Ltd.

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

[snowplow-repo]: https://github.com/snowplow/snowplow
[snowplow-website]: http://snowplowanalytics.com

[vagrant-install]: http://docs.vagrantup.com/v2/installation/index.html
[virtualbox-install]: https://www.virtualbox.org/wiki/Downloads

[techdocs-image]: https://d3i6fms1cm1j0i.cloudfront.net/github/images/techdocs.png
[setup-image]: https://d3i6fms1cm1j0i.cloudfront.net/github/images/setup.png
[roadmap-image]: https://d3i6fms1cm1j0i.cloudfront.net/github/images/roadmap.png
[contributing-image]: https://d3i6fms1cm1j0i.cloudfront.net/github/images/contributing.png

[techdocs]: https://github.com/snowplow/iglu/wiki/Scala-client
[setup]: https://github.com/snowplow/iglu/wiki/Scala-client-setup
[roadmap]: https://github.com/snowplow/iglu/wiki/Product-roadmap
[contributing]: https://github.com/snowplow/iglu/wiki/Contributing

[license]: http://www.apache.org/licenses/LICENSE-2.0
