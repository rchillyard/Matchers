# Matchers Library — Typelevel Submission Considerations

*Number Library — Architecture & Planning — March 2026*

---

## Overview

The `Matchers` library is a general-purpose combinator library for pattern
matching and transformation over arbitrary input types. It was designed as a
generalisation of Scala's `ParserCombinators`, replacing the fixed `Input`
type (always a `CharSequence`) with a parametric type `T`. The result is a
single coherent abstraction that unifies parsing (where `T = String`) and
tree transformation (where `T = Expression`, `Aggregate`, `DyadicTriple`,
etc.).

The core trait is a single line:

```scala
trait Matcher[-T, +R] extends (T => MatchResult[R])
```

Contravariant in `T`, covariant in `R` — exactly the right variance for a
function-like abstraction.

---

## The Killer Feature

The parametric input type `T` is the key differentiator from every existing
combinator library in the Typelevel ecosystem. A `Matcher[String, T]` is a
parser. A `Matcher[Expression, Expression]` is a simplifier. A
`Matcher[Aggregate, Expression]` is a scoped transformation that only applies
to a specific subtype. A `Matcher[DyadicTriple, Expression]` is a rule over
binary expression trees.

All of these share the same combinator infrastructure: `|`, `&`, `~`, `~~`,
`flatMap`, `map`, `filter`, `^^`, `sequence`, `sequenceStrict`, and more.

This is the pitch: **`Matchers` is to transformation what `cats-parse` is to
parsing — but with the input type abstracted away.**

---

## Comparison with Existing Typelevel Libraries

### `cats-parse`
The most obvious comparison. `cats-parse` is excellent for parsing
`CharSequence` input, but the input type is fixed. `Matchers` generalises
this to any `T`, making it applicable to any domain where you need
composable, fallible transformations with success/failure/error semantics.

### `cats` core
The `Matcher` typeclass instances (Functor, Monad, etc.) align naturally with
`cats` typeclasses. A `Matcher[-T, +R]` is a `Monad` in `R` and can
potentially participate in the `cats` typeclass hierarchy. This alignment
would need to be formalised before submission.

---

## Three-Way Result Distinction

Unlike parser combinators which have two outcomes (`Success`/`Failure`),
`Matchers` has three:

- **`Match(r)`** — the transformation succeeded, producing `r`
- **`Miss(msg, t)`** — the rule did not apply (expected, recoverable)
- **`Error(e)`** — an exception occurred (unexpected, propagates)

This distinction is more principled than two-way failure. In a simplifier,
"this rule doesn't apply" (`Miss`) is qualitatively different from
"a divide-by-zero occurred mid-simplification" (`Error`). The `~~` operator
(lenient `andThen`) exploits this — it succeeds even if one side is a `Miss`,
using the miss value as the result. This has no parser combinator analogue but
is very natural for tree transformation.

---

## Notable Combinators

| Combinator | Description |
|---|---|
| `\|` | Ordered choice — try left, fall back to right on `Miss` |
| `&` | Sequential — run left, then right on same input |
| `~` | Product — run left and right on respective halves of a `~` pair |
| `~~` | Lenient product — succeeds even if one side is a `Miss` |
| `~>` / `<~` | Directed product — keep only right / left result |
| `flatMap` / `map` | Monadic chaining |
| `^^` | Synonym for `map` (Parser Combinators style) |
| `sequence` | Apply a matcher to each element of a `Seq`, keeping misses |
| `sequenceStrict` | Apply a matcher to each element, failing on first miss |
| `opt` / `?` | Optional match — returns `Option[R]` |
| `trial` | Returns `Try[R]` |
| `valve` | Guards a match with a predicate |
| `chain` | Chains a matcher with a validating matcher |
| `alt` | Always succeeds — returns match result or original input |

---

## Strengths for Typelevel Submission

- **Correct variance** — `[-T, +R]` is principled and type-safe
- **Principled three-way result** — `Match`/`Miss`/`Error` is more expressive
  than two-way success/failure
- **Already separately published** — packaging hygiene is in place
- **Battle-tested** — used extensively in the Number library for both parsing
  (`LaTeXParser`, `ExpressionParser`, `UnitsParser`, `QuantityParser`) and
  symbolic simplification (five-phase pipeline over expression trees)
- **Familiar idiom** — the `~`, `|`, `^^` combinators will be immediately
  recognisable to anyone who has used Scala Parser Combinators
- **Logging built in** — the `:| ` operator attaches a name for tracing,
  with configurable log levels (`LogOff`, `LogInfo`, `LogDebug`)

---

## Things to Address Before Submission

### 1. Overlap with `cats-parse`
Prepare a clear, concise articulation of the differentiation. The parametric
input type is the headline. Secondary points: three-way result, `~~` operator,
and the tree-transformation use case.

### 2. Typeclass laws
Typelevel requires lawful typeclass instances. The `Matcher` monad instance
needs laws defined and verified, ideally using `discipline`. Currently the
library is tested behaviourally (via `MatchersSpec`) but not law-checked.

### 3. Name collision
`Matchers` conflicts with ScalaTest's `Matchers` trait when both are in
scope. Consider renaming the library or the core trait — possibilities
include `Transformer`, `Transducer`, or `Rule`. Alternatively, a package
rename may be sufficient.

### 4. Documentation
Typelevel expects comprehensive Scaladoc and ideally a microsite. The
`MatchersSpec` is an excellent source of examples to draw from — many of the
test cases read as documentation already.

### 5. Scala 3 idioms
Ensure the library makes full use of Scala 3 features (given/using,
extension methods, opaque types where appropriate) and that the API is
idiomatic Scala 3 rather than a direct port of Scala 2 style.

---

## Recommended Next Steps

1. **Now:** note this document as a future work item; keep focus on
   Number library Version 1.8
2. **Before submission:** write a short comparison document positioning
   `Matchers` against `cats-parse`
3. **Before submission:** add `discipline`-based law checks for `Functor`
   and `Monad` instances
4. **Before submission:** resolve the name collision question
5. **Submission:** open a discussion on the Typelevel Discord (`#contributors`
   channel) before filing a formal proposal — this is the standard route for
   new affiliate libraries

---

## Summary

The `Matchers` library is a genuinely novel contribution to the Scala
ecosystem. The parametric input type is a simple but powerful generalisation
that unlocks a whole class of use cases beyond parsing. With some law-checking
work and documentation investment, it is a strong candidate for Typelevel
affiliate status. The priority for now is Version 1.8 of the Number library —
but this is worth revisiting once that milestone is reached.