### 2.0.0

* Scala 3 support
* Upgrade Scala.JS from 0.6.33 to 1.6
* Replace Scalaz with Cats
* Drop Scala 2.12 support


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
