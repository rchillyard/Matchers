# Code Review: `Matchers.scala`

_Review conducted after fixing the `eitherOr` over-evaluation bug (2026-03-09)._

---

## 1. `**` combinator re-invokes `m` up to three times

```scala
def **[T, R](m: Matcher[T ~ T ~ T, R], commutes: Boolean = true): Matcher[T ~ T ~ T, R] =
  Matcher("**")(m | (maybe[T ~ T ~ T](commutes) & rotate3 & m) | (maybe[T ~ T ~ T](commutes) & invert3 & m))
```

`m` appears three times. This is structurally the same pattern as the old `eitherOr` bug, though here the inputs differ between arms (original, rotated, inverted), so it is not strictly redundant. However, if `m` is expensive, unnecessary invocations still matter. The inputs are distinct so the fix is less straightforward, but the structure is worth a `CONSIDER`.

---

## 2. `Miss.map` overrides a default that is already correct

```scala
override def map[S](f: R => S): MatchResult[S] = Miss(msg, t)
```

The base `MatchResult.map` delegates to `flatMap(r => success(f(r)))`, and `Miss.flatMap` already returns `Miss(msg, t)`. The override is harmless but redundant. Either remove it, or add a `// NOTE: micro-optimisation avoiding flatMap allocation` comment to explain the intent.
NOTE this item has been resolved in V1.0.16.
---

## 3. `MatchResult.apply(r: R, o: R)` is unfinished and has a stray `System.err.println`

```scala
def apply[R](r: R, o: R): MatchResult[R] = {
  if (r == o) System.err.println(s"Match: $r (unchanged)")
  Match(r)
}
```

This always returns `Match(r)` regardless of whether `r == o`. The `println` is clearly a debug artifact, and the parameter `o` is effectively unused in the result. Either this method should return `Miss` when the values are unequal (making it a proper equality check), or the whole method should be deleted. As it stands it is misleading and the `println` must not reach a release.

---

## 4. `Miss.guard` and `Miss.andThen` allocate a new `Miss` instead of returning `this`

```scala
def guard[S](sm: => MatchResult[S]): MatchResult[S] = Miss(msg, t)
def andThen[S](sm: => MatchResult[S]): MatchResult[R ~ S] = Miss(msg, t)
```

Both construct a fresh `Miss` object where `this` would suffice (the existing `// NOTE: essentially this` comment appears on `&` but not here). Minor allocation concern, but worth consistency. Either cast and return `this`, or add the same `NOTE` comment so the intent is clear.
NOTE this item has been resolved in V1.0.16.

---

## 5. `isEqual` has an unresolved "not sure why" scaladoc

```scala
/** Not sure why we need this but it's here. */
def isEqual[R](q: R, r: R): Boolean = q == r
```

This uncertainty should be resolved. If the method exists to allow override for custom equality semantics, that should be documented. If it has no purpose, it should be removed. "Not sure why we need this" is not acceptable in code intended for a Typelevel submission.
NOTE this item has been resolved in V1.0.16.

---

## 6. Copy-paste error in `match3Any` scaladoc

```scala
* @tparam T2 the input type for the second Matcher
```

Should read "third Matcher." Minor, but scaladoc errors in a library are visible to users.

---

## 7. Commented-out `commutative` combinator contains broken syntax

Lines 1708–1717 contain a commented-out `commutative` method with incomplete and syntactically invalid code. This should either be deleted entirely or replaced with a properly filed `// TODO` referencing a tracking issue.

---

## 8. The recursive-matcher guard in `|` carries unresolved uncertainty

```scala
// CONSIDER this is an attempt to avoid infinite recursion. No idea if it works.
case _ if m != this =>
  m(t)
case _ =>
  throw MatcherException("recursive matcher")
```

"No idea if it works" must not remain in production code, and especially not in a library. Either:
- validate the guard with a test that would fail without it, and replace the comment with a `// NOTE` explaining what it protects against; or
- file a `FIXME` with a concrete description of the scenario that triggers infinite recursion (Issue #14 is mentioned in the scaladoc — that reference should appear here too).

This is the highest-priority item for a Typelevel submission.