# ClearConfig
[![Build Status](https://travis-ci.org/japgolly/clear-config.svg?branch=master)](https://travis-ci.org/japgolly/clear-config)

Doc coming soon...

# What's special about this Config library?

There are plenty of config libraries out there, right?
This library is pure FP, type-safe, super-simple to use, highly composable and powerful, yada yada yada...
All true but it's biggest and most unique feature is actually:

**<span style="color:#d00">CLARITY.</span>**

Haven't we all had enough of crap like:

* changing an environment variable setting, pushing all the way though to an environment, testing and
  then discovering that your expected change didn't occur. Was the new setting picked up?
  What setting did it use? Where did it come from?

* after hours of frustration: "That setting isn't even used any more?!
  Why the hell is it still all over our deployment config?!
  Why didn't person X remove this specific piece of text in this big blob of text in this completely
  separate deployment repo at the same time they made their code change?"

This library endeavours to provide **clarity**.
When you get an instance of your config, you also get a report that describes:

* where config comes from
* what values each config source provides
* how config sources override other sources
* what config keys are in use
* what the ultimate config (value) is
* which config is still hanging around is actually stale and no longer in use


# Walkthrough

Here's a pretty common scenario that will serve as a decent introduction.

We have an app which has the following config:

```scala
import java.net.URL

final case class DatabaseConfig(
  port     : Int,
  url      : URL,
  username : String,
  password : String,
  schema   : Option[String])
```

Let's define how we populate our config from the outside world...

```scala
import japgolly.clearconfig._
import scalaz.syntax.applicative._

object DatabaseConfig {

  def config: ConfigDef[DatabaseConfig] =
    (
      ConfigDef.getOrUse("PORT", 8080)   |@|
      ConfigDef.need[URL]("URL")         |@|
      ConfigDef.need[String]("USERNAME") |@|
      ConfigDef.need[String]("PASSWORD") |@|
      ConfigDef.get[String]("SCHEMA")
    )(apply)
}
```

Great, now let's define where we want to reading config from.
At this point you also need to decide which effect to use.
For simplicity, we'll just use `Id` and opt-out of safe FP.

```scala
import scalaz.Scalaz.Id

def configSources: ConfigSources[Id] =
  // Highest priority
  ConfigSource.environment[Id] >
  ConfigSource.propFileOnClasspath[Id]("/database.props", optional = true) >
  ConfigSource.system[Id]
  // Lowest priority
```

Now we're ready to create a real instance based on the real environment.

```scala
val dbCfg: DatabaseConfig =
  DatabaseConfig.config
    .run(configSources)
    .getOrDie() // Just throw an exception if necessary config is missing
```

Done! But so far we're not using the most important feature of the library: the report.

Let's get and print out a report at the end.

1. We'll remove env & system from unused keys to keep the report small seeing as it's just a demo.
1. We'll also prefix all keys by `POSTGRES` to make it look a bit more realistic.

```scala
println(report

  // Remove env & system columns from the unused section of the report
  .mapUnused(_.withoutSources(ConfigSourceName.environment, ConfigSourceName.system))

  // Show the full report
  .full
)
```

Sample output:

```text
4 sources (highest to lowest priority):
  - Env
  - cp:/database.properties
  - System
  - Default

Used keys (5):
+-------------------+------+-------------------------+---------+
| Key               | Env  | cp:/database.properties | Default |
+-------------------+------+-------------------------+---------+
| POSTGRES_PASSWORD |      | <# 1C02B9F6 #>          |         |
| POSTGRES_PORT     | 4000 |                         | 8080    |
| POSTGRES_SCHEMA   |      |                         |         |
| POSTGRES_URL      |      | http://localhost/blah   |         |
| POSTGRES_USERNAME |      | demo                    |         |
+-------------------+------+-------------------------+---------+

Unused keys (1):
+----------------+-------------------------+
| Key            | cp:/database.properties |
+----------------+-------------------------+
| POSTGRES_SCHMA | public                  |
+----------------+-------------------------+
```

From the above report we can immediately observe the following:

* Which sources override other sources; the report columns (left-to-right) respect this
* We'll be running at port 4000 and the reason for that is there's an override set by the environment
* There's a typo in our `database.properties`; `POSTGRES_SCHMA` should be `POSTGRES_SCHEMA`

# Usage

* Simplest and most common methods:

  ```scala
  ConfigDef.get     [A](key: String)             // provides an Option[A] - optional config
  ConfigDef.getOrUse[A](key: String, default: A) // provides an A - optional config with default
  ConfigDef.need    [A](key: String)             // provides an A - mandatory config; error if not provided
  ```

* To define your own type of config value, create an implicit `ConfigValueParser`. Example:

  ```scala
  sealed trait MyBool
  case object Yes extends MyBool
  case object No extends MyBool

  implicit val myBoolParser: ConfigValueParser[MyBool] =
    ConfigValueParser.oneOf[MyBool]("yes" -> Yes, "no" -> No)
      .preprocessValue(_.toLowerCase) // Make it case-insensitive
  ```

* Call `.secret` on your `ConfigDef` to force it to be obfuscated in the report.
  The default (implicit) report settings already obfuscate keys that contain substrings like
  `password`, `credential`, and `secret`. Override `configReportSettings` if required.

* Shell-style Comments (beginning with `#`) are automatically removed from config values.
  Create your own implicit `ConfigValuePreprocessor` to customise this behaviour.

# Can I use this with Cats?

Yes and I do on some projects. Add [shims](https://github.com/djspiewak/shims) and it's as simple as:

```scala
import shims._
```
