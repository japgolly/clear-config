### 3.3.0

* When calling `report.map{Used,Unused}`:
  * Added `filterKeysAndSource(f: (String, SourceName) => Boolean)`
  * Added `filterKeysAndSourceNot(f: (String, SourceName) => Boolean)`
  * Added `sources: SourceName*` to `filterKeys` to only filter keys for a given set of sources
  * Added `sources: SourceName*` to `filterKeysNot` to only filter-not keys for a given set of sources
  * Added `withoutKeys(keys: Set[String], sources: SourceName*)` to remove keys for a given set of sources, or all if none specified

### 3.2.0

* [#133](https://github.com/japgolly/clear-config/issues/133) Add `ConfigDef.logbackXmlOnClasspath` (available on JVM only) that scans logback xml files for environment variables so that they can appear in a config report.
* [#127](https://github.com/japgolly/clear-config/issues/127) Add `show(unused: Boolean): String` to config reports.

### 3.1.0

* Add `ConfigDef.getOrParse{,OrThrow}`. They're the same as `getOrUse` except the default value is parsed instead of provided.
* In `ConfigDef` instances...
  * Deprecate `.option` and add `.whenAtLeastOneKeySpecified` as a replacement
  * Add `.whenFullySpecified`
* Add implicit `ConfigValueParser[InetAddress]`
* Add implicit `ConfigValueParser[UUID]`
* Upgrade dependencies

### 3.0.0

* Set `versionScheme` to `"early-semver"`
* Upgrade `microlibs` to 4.0.0
* Update Scala.js to 1.7
* Better clarify in reports where values are obfuscated. Instead of `<# hash #>`, obfuscated values are now displayed as `Obfuscated (hash)`

### 2.0.0

* Scala 3 support
* Upgrade Scala.JS from 0.6.33 to 1.6
* Replace Scalaz with Cats
* Drop Scala 2.12 support
* The JS module now includes ability to read environment (if in Node) and system properties
* Added to `ConfigSource` instances:
  * `expandInlineProperties(key)` for when a single value is the contents of a properties file
  * `treatKeyDotsAsUnderscores`
* `ConfigSource.environment` now longer calls `treatKeyDotsAsUnderscores` automatically.
  Use `ConfigSource.environment[F].treatKeyDotsAsUnderscores` if required.
* Expose `ConfigKey`
* Add new implicit `ConfigValueParser` instances for:
  * `java.util.regex.Pattern`
  * `scala.util.matching.Regex`

### 1.4.0

* Scala 2.13 support

* Added `ConfigSource.expandInlineProperties` to accept an entire properties file as the value of a single key. (JVM only)

  For example, you could have Terraform read an entire file and `jsonencode` it as a single environment variable key
  so that your entire config is in an AWS ECS task definition, and you don't need to update any Terraform code when
  you add/remove/modify property keys.


### 1.3.0

* Support users passing in null values (eg. you're configuring a Java library) without crashing
* `ConfigValueParser` now just simply parses a `String` rather than an internal object with key and value

### ≤1.2.2

* See git history
