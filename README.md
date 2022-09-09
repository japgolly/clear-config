# ClearConfig
[![Build Status](https://travis-ci.org/japgolly/clear-config.svg?branch=master)](https://travis-ci.org/japgolly/clear-config) [![Latest version](https://index.scala-lang.org/japgolly/clearconfig/core/latest.svg?color=orange)](https://index.scala-lang.org/japgolly/clearconfig/core)

A type-safe, FP, Scala config library.

```scala
libraryDependencies += "com.github.japgolly.clearconfig" %%% "core" % "<ver>"
```


# What's special about this?

There are plenty of config libraries out there, right?
This library is pure FP, type-safe, super-simple to use, highly composable and powerful, yada yada yada...
All true but it's biggest and most unique feature is actually:

**CLARITY.**

Haven't we all had enough of crap like:

* changing an environment variable setting, pushing all the way though to an environment, testing and
  then discovering that your expected change didn't occur. Was the new setting picked up?
  What setting did it use? Where did it come from?

* after hours of frustration: "That setting isn't even used any more?!
  Why the hell is it still all over our deployment config?!
  Why didn't person X magically know to remove this specific piece of text in this big blob of text in this completely
  separate deployment repo at the same time they made their code change?"

This library endeavours to provide **clarity**.
When you get an instance of your config, you also get a report that describes:

* where config comes from
* how config sources override other sources
* what values each config source provided
* what config keys are in use
* what the total, resulting config is
* which config is still hanging around but is actually stale and no longer in use

*(sample report below)*


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
import cats.implicits._

object DatabaseConfig {

  def config: ConfigDef[DatabaseConfig] =
    (
      ConfigDef.getOrUse("PORT", 8080),
      ConfigDef.need[URL]("URL"),
      ConfigDef.need[String]("USERNAME"),
      ConfigDef.need[String]("PASSWORD"),
      ConfigDef.get[String]("SCHEMA")
    ).mapN(apply)
}
```

Great, now let's define where we want to read config from.
At this point you also need to decide which effect type to use.
You'd typically use something like `IO` but for simplicity,
we'll just use `Id` and opt-out of safe FP.

```scala
import cats.Id

def configSources: ConfigSources[Id] =
  ConfigSource.environment[Id] >                                             // Highest priority
  ConfigSource.propFileOnClasspath[Id]("/database.props", optional = true) > //
  ConfigSource.system[Id]                                                    // Lowest priority
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
1. We'll also prefix all keys by `POSTGRES_` to make it look a bit more realistic.

```scala
val (dbCfg, report) =
  DatabaseConfig.config
    .withPrefix("POSTGRES_")
    .withReport
    .run(configSources)
    .getOrDie() // Just throw an exception if necessary config is missing

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
| POSTGRES_PASSWORD |      | Obfuscated (1C02B9F6)   |         |
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
* The password value has been hashed for the report. This still allows you to compare the hash between envs or time to determine change without compromising the value.


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

* Keys can be modified after the fact. Eg. `ConfigDef.get("A").withPrefix("P_").withKeyMod(_.replace('_', '.'))`
  is equivalent to `ConfigDef.get("P.A")`. This also works when a `ConfigDef` is a composition of more than
  one key, in which case they'll all be modified.

* There is special DSL to create `A => Unit` functions to configure a mutable object (which you typically use when working with a Java library)
  `ConfigDef.consumerFn[A](...)`. There is an example below:

* More... (explore the source)

# Larger Example

You typically compose using `Applicative`, give the composite a prefix,
then use (nest) it in some higher-level config.

For example, this Scala code...

```scala
import cats.syntax.apply._
import japgolly.clearconfig._
import java.net.{URI, URL}
import redis.clients.jedis.JedisPoolConfig

case class AppConfig(postgres: PostgresConfig, redis: RedisConfig, logLevel: LogLevel)

object AppConfig {
  def config: ConfigDef[AppConfig] =
    ( PostgresConfig.config,
      RedisConfig.config,
      ConfigDef.getOrUse("log_level", LogLevel.Info)
    ).mapN(apply)
      .withPrefix("myapp.")
}

case class PostgresConfig(url: URL, credential: Credential, schema: Option[String])

object PostgresConfig {
  def config: ConfigDef[PostgresConfig] =
    ( ConfigDef.need[URL]("url"),
      Credential.config,
      ConfigDef.get[String]("schema"),
    ).mapN(apply)
      .withPrefix("postgres.")
}

case class Credential(username: String, password: String)

object Credential {
  def config: ConfigDef[Credential] =
    ( ConfigDef.need[String]("username"),
      ConfigDef.need[String]("password"),
    ).mapN(apply)
}

case class RedisConfig(uri: URI, credential: Credential, configurePool: JedisPoolConfig => Unit)

object RedisConfig {

  def poolConfig: ConfigDef[JedisPoolConfig => Unit] =
    ConfigDef.consumerFn[JedisPoolConfig](
      _.get("block_when_exhausted", _.setBlockWhenExhausted),
      _.get("eviction_policy_class_name", _.setEvictionPolicyClassName),
      _.getOrUse("fairness", _.setFairness)(true),
      _.get("jmx_enabled", _.setJmxEnabled),
      _.get("jmx_name_base", _.setJmxNameBase),
      _.get("jmx_name_prefix", _.setJmxNamePrefix),
      _.get("lifo", _.setLifo),
      _.get("max_idle", _.setMaxIdle),
      _.get("max_total", _.setMaxTotal),
      _.get("max_wait_millis", _.setMaxWaitMillis),
      _.get("min_evictable_idle_time_millis", _.setMinEvictableIdleTimeMillis),
      _.getOrUse("min_idle", _.setMinIdle)(2),
      _.get("num_tests_per_eviction_run", _.setNumTestsPerEvictionRun),
      _.get("soft_min_evictable_idle_time_millis", _.setSoftMinEvictableIdleTimeMillis),
      _.get("test_on_borrow", _.setTestOnBorrow),
      _.get("test_on_create", _.setTestOnCreate),
      _.get("test_on_return", _.setTestOnReturn),
      _.get("test_while_idle", _.setTestWhileIdle),
      _.get("time_between_eviction_runs_millis", _.setTimeBetweenEvictionRunsMillis)
    )

  def config: ConfigDef[RedisConfig] =
    ( ConfigDef.need[URI]("uri"),
      Credential.config,
      poolConfig.withPrefix("pool."),
    ).mapN(apply)
      .withPrefix("redis.")
}

sealed trait LogLevel
object LogLevel {
  case object Debug extends LogLevel
  case object Info extends LogLevel
  case object Warn extends LogLevel

  implicit def configValueParser: ConfigValueParser[LogLevel] =
    ConfigValueParser.oneOf[LogLevel]("debug" -> Debug, "info" -> Info, "warn" -> Warn)
      .preprocessValue(_.toLowerCase)
}
```

will read the following properties:

```
myapp.postgres.password
myapp.postgres.schema
myapp.postgres.url
myapp.postgres.username

myapp.redis.password
myapp.redis.uri
myapp.redis.username

myapp.redis.pool.block_when_exhausted
myapp.redis.pool.eviction_policy_class_name
myapp.redis.pool.fairness
myapp.redis.pool.jmx_enabled
myapp.redis.pool.jmx_name_base
myapp.redis.pool.jmx_name_prefix
myapp.redis.pool.lifo
myapp.redis.pool.max_idle
myapp.redis.pool.max_total
myapp.redis.pool.max_wait_millis
myapp.redis.pool.min_evictable_idle_time_millis
myapp.redis.pool.min_idle
myapp.redis.pool.num_tests_per_eviction_run
myapp.redis.pool.soft_min_evictable_idle_time_millis
myapp.redis.pool.test_on_borrow
myapp.redis.pool.test_on_create
myapp.redis.pool.test_on_return
myapp.redis.pool.test_while_idle
myapp.redis.pool.time_between_eviction_runs_millis

myapp.log_level
```


##### Support:
If you like what I do
—my OSS libraries, my contributions to other OSS libs, [my programming blog](https://japgolly.blogspot.com)—
and you'd like to support me, more content, more lib maintenance, [please become a patron](https://www.patreon.com/japgolly)!
I do all my OSS work unpaid so showing your support will make a big difference.
