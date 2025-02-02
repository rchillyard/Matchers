[![Codacy Badge](https://app.codacy.com/project/badge/Grade/454847cac6214df8986ef2dec68e5049)](https://www.codacy.com/gh/rchillyard/Matchers/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=rchillyard/Matchers&amp;utm_campaign=Badge_Grade)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.phasmidsoftware/matchers_2.13/badge.svg?color=blue)](https://maven-badges.herokuapp.com/maven-central/com.phasmidsoftware_2.13/matchers/)
[![CircleCI](https://circleci.com/gh/rchillyard/Matchers.svg?style=svg)](https://circleci.com/gh/rchillyard/Matchers)
![GitHub Top Languages](https://img.shields.io/github/languages/top/rchillyard/Matchers)
![GitHub](https://img.shields.io/github/license/rchillyard/Matchers)
![GitHub last commit](https://img.shields.io/github/last-commit/rchillyard/Matchers)
![GitHub issues](https://img.shields.io/github/issues-raw/rchillyard/Matchers)
![GitHub issues by-label](https://img.shields.io/github/issues/rchillyard/Matchers/bug)

# Matchers
A library of composable matchers.
If you're familiar with the _Parsers_ class from the Scala parser-combinators library, you should be fairly
comfortable with the **Matchers**.

The chief difference between _Parsers_ and **Matchers** is that, whereas _Parsers_ defines two parametric types via
the type alias mechanism and one parametric type of the _ParseResult_ type.
In **Matchers**, there are just the two parametric types on the _MatchResult_ type.

For the most part, the methods in **Matchers** result in a _Matcher_[_T_, _R_] where _T_ and _R_ are the input and result types
respectively.
A _Matcher_[T, R] is a function which takes a value of _T_ and returns a _MatchResult_[_R_].
There are three subclasses of _MatchResult_[_R_]:
* case class Match[R](r: R)
* case class Miss[T, R](msg: String, t: T)
* case class Error[R](e: Throwable)

# Composition of MatchResult
Perhaps the simplest place to start is with the || method:

    def ||[S >: R](sm: => MatchResult[S]): MatchResult[S]

If _this_ is successful, then it is returned as is.
Otherwise, _sm_ will be returned.
Note that _MatchResult[R]_ is a subtype of _MatchResult[S]_ because the parametric type of _MatchResult_ is covariant,
and because _R_ is a subtype of _S_.

There is a similar method defined as |:

    def |[S >: R](sm: => Matcher[Any, S]): MatchResult[S]

It behaves as || if _this_ is successful, but otherwise, matcher _sm_ is invoked on the failed input value for this _Miss_.
An _Error_ always returns itself.

The corresponding "and" methods are somewhat different because the return
type must include two values of two disparate types:

    def &&[S](sm: => MatchResult[S]): MatchResult[R ~ S]

The result of the && method will be successful only if _sm_ is also successful.
In this case, the result will a tuple of the two results.

# Parsing
Any _Matcher_ whose input is a _String_ can be referred to as a _Parser_ (a type alias).

There are simple numeric parsers, for example:

    val p = m.parserInt
    p("12345") shouldBe m.Match(12345)

It is easy to create matchers which parse regular expressions (without having to depend on Scala Parser Combinators).
For example:

    import matchers._
    val m: matchers.Parser[List[String]] = """(\w+)\s(\d+)""".regexGroups
    m("Hello 12345") shouldBe matchers.Match(List("Hello", "12345"))

This utilizes the _regexGroups_ method of implicit class _ParserOps_.

It is also easy to parse strings as instances of case classes, even with optional parameters.
See, for example,

    case class Rating(code: String, age: Option[Int])
    val p: m.Parser[Rating] = m.parser2("""(\w+)(-(\d+))?""", 1, 3)(m.always, m.opt(m.parserInt))(Rating)
    p("PG-13") shouldBe m.Match(Rating("PG", Some(13)))
    p("R") shouldBe m.Match(Rating("R", None))

# Tilde
As in the Scala Parser Combinators, there is a ~ case class.
It is essentially just a tuple of two elements.
However, if we use the ~ operator on two _Matchers_ for example as follows:

    import matchers._
    val m: matchers.Matcher[String ~ String, Int] = "1".m ~ "2".m ^^ {
      case x ~ y => x.toInt + y.toInt
    }

... you can use the ^^ (or _map_) method to transform the _MatchResult_ from _String_ ~ _String_ to some other type,
in this case an _Int_.

In this case, the _m_ method on a _String_ is provided by an implicit class _MatcherStringOps_.

# Usage
Typical examples of the use of **Matchers** would be something such as the following
(from an application which deals with lazy expressions of numeric quantities):

    def biFunctionSimplifier: Matcher[Expression, Expression] = matchBiFunction & (matchSimplifyPlus | matchSimplifyTimes) :| "biFunctionSimplifier"

This can be interpreted as defining a matcher which takes an _Expression_ and returns an _Expression_.
If _matchBiFunction_ succeeds,
AND if one of the following matchers succeeds (tried in sequence), then a _Match[Expression]_ will result.
The intermediate result is a Tuple3.
The final _:| "biFunctionSimplifier"_ is a logging matcher which can be turned on for easier debugging but otherwise does not affect
the result.

    def matchSimplifyPlus: Matcher[(ExpressionBiFunction, Expression, Expression), Expression] =
      matchDyadicBranches(Sum) & *(matchBiFunctionConstantResult(Product, Number(-1), Number.zero)) :| "matchSimplifyPlus"

This can be interpreted as defining a matcher which takes the tuple returned by the _matchBiFunction_ above,
and returns an _Expression_.
If _matchDyadicBranches(Sum)_ succeeds (where _Sum_ is basically the "plus" operator),
AND if the following matcher succeeds, then a _Match[Expression]_ will result.
The intermediate result is a tuple of _Expressions_.
Since these are in arbitrary order, the "*" matcher will try _matchBiFunctionConstantResult(Product, Number(-1), Number.zero)_
with the tuple straight or inverted as necessary.

Debugging/Logging
=================
By default, matchers are not named (the default name is "").
it is easy to name matchers, either by using the _Matchers_ method (with name parameter) or the following mechanism:
After any matcher reference, you can invoke the :| operator with a String representing the name and this will
turn on logging for just that .
For example,

    import m.MatcherOps
    val p = m.success(1) :| "success(1)"

Note that somewhere you will have to import the _MatcherOps_ in order to make the :| operator available.

The second change you need to make to enable debugging/logging is to set the _LogLevel_ by defining an implicit val of that type.
The default value is _LogOff_, but you also have _LogDebug_ and _LogInfo_ available.

The default _MatchLogger_ results in logging information going to the console via _println_.
You can easily set up your own implicit value of _MatchLogger_ which is simply a _String => Unit_ function.

Version
=======
* 1.0.6 Match sequences.
* 1.0.5 Added combine and accumulate methods to MatchResult...
* 1.0.4 Make logging more consistent and easier to use.
* 1.0.3 Replaced most tuples with tildes.
* 1.0.2 Support regex parsers
* 1.0.1 Added ~
* 1.0.0 Original Version