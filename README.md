# Microlibraries!
[![Build Status](https://travis-ci.org/japgolly/microlibs-scala.svg?branch=master)](https://travis-ci.org/japgolly/microlibs-scala)


```scala
val VerMicrolibs = "1.0"

"com.github.japgolly.microlibs" %% "adt-macros"  % VerMicrolibs
"com.github.japgolly.microlibs" %% "macro-utils" % VerMicrolibs
"com.github.japgolly.microlibs" %% "name-fn"     % VerMicrolibs
"com.github.japgolly.microlibs" %% "nonempty"    % VerMicrolibs
"com.github.japgolly.microlibs" %% "recursion"   % VerMicrolibs
"com.github.japgolly.microlibs" %% "scalaz-ext"  % VerMicrolibs
"com.github.japgolly.microlibs" %% "stdlib-ext"  % VerMicrolibs
```

Contents:

| Module | Version | Deps |
|--------|---------|------|
| MacroUtils | unreleased | - |
| NonEmpty | unreleased | Scalaz, UnivEq |
| Recursion | unreleased | Scalaz |

# Module: MacroUtils

TODO

# Module: NonEmpty

* NonEmpty (arbitrary)
* NonEmptySet
* NonEmptyVector

# Module: Recursion

A simple performant version of recursion schemes.

Uses `Fix` only; no `Mu`, `Nu`, or `{Cor,R}ecursion` typeclasses.
