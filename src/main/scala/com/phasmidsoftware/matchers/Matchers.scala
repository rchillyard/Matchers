package com.phasmidsoftware.matchers

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

/**
  * This trait defines a set of Matchers which operate in a parallel fashion to the Parsers of the Scala
  * Parser Combinator library.
  */
trait Matchers {

  matchers =>

  /**
    * Method to create a Matcher, based on the given function f of form T => MatchResult[R].
    * Unusually, this method's identifier has a capital first letter.
    * This is done to mimic the Parser method in the parser-combinators.
    * It has the advantage of looking somewhat like a constructor of a Matcher.
    *
    * NOTE: if you are looking for a method which takes a function f of form T => R, then you need to use the lift method.
    *
    * If f is a partial function, it may result in a MatchError.
    * In such a case, this is simply treated as a Miss.
    * However, if f throws some other non-fatal exception, then that will result in an Error.
    *
    * @param f a T => MatchResult[R].
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T, R] based on f.
    */
  def Matcher[T, R](f: T => MatchResult[R]): Matcher[T, R] = constructMatcher(f)

  /**
    * Method to create a named Matcher, based on the given function f.
    *
    * @param name the name for the logger to mention.
    * @param f    a T => MatchResult[R].
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T, R] based on f.
    */
  def namedMatcher[T, R](name: => String)(f: T => MatchResult[R])(implicit ll: LogLevel, logger: MatchLogger): Matcher[T, R] = Matcher(f) :| name

  /**
    * Matcher based on the function f.
    *
    * @param f a function of T => R
    * @tparam T the input type to both f and the resulting Matcher.
    * @tparam R the result type to both f and the resulting Matcher.
    * @return a Matcher[T, R].
    */
  def lift[T, R](f: T => R): Matcher[T, R] = t => MatchResult(f(t))

  /**
    * Matcher which always succeeds and creates a Match with value r.
    *
    * @param r the predetermined result.
    * @tparam T the input type (input is ignored).
    * @tparam R the result type.
    * @return a Matcher[T, R]
    */
  def success[T, R](r: => R): Matcher[T, R] = _ => MatchResult(r)

  /**
    * Matcher which always fails and creates a Miss with the value tried.
    *
    * @param msg the message to describe this failure.
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T, R]
    */
  def fail[T, R](msg: String): Matcher[T, R] = t => Miss(msg, t)

  /**
    * Matcher which always fails and creates a Miss with the value tried.
    *
    * @param e a Throwable.
    * @tparam R the result type.
    * @return a Matcher[T, R]
    */
  def error[R](e: Throwable): Matcher[Any, R] = _ => Error(e)

  /**
    * Matcher which always succeeds and whose input type and result type are the same.
    *
    * @tparam R both the input type and the result type.
    * @return a Matcher[R, R] which always succeeds.
    */
  def always[R]: Matcher[R, R] = lift(identity)

  /**
    * Matcher which succeeds only if the predicate p evaluates to true.
    *
    * @param p a predicate on type R.
    * @tparam R both the input type and the result type.
    * @return a Matcher[R, R] which succeeds only if p(r) is true.
    */
  def filter[R](p: R => Boolean): Matcher[R, R] = r => if (p(r)) Match(r) else Miss("filter", r)

  /**
    * Matcher which succeeds only if the predicate p evaluates to true.
    *
    * @param b a constant Boolean value.
    * @tparam R both the input type and the result type.
    * @return a Matcher[R, R] which succeeds only if p(r) is true.
    */
  def maybe[R](b: Boolean): Matcher[R, R] = filter(_ => b)

  /**
    * Matcher which succeeds if the input is equal to the given t.
    *
    * @param t the value which must be matched.
    * @tparam T the type of the input and result for Matcher.
    * @return a Matcher[T, T].
    */
  def matches[T](t: T): Matcher[T, T] = filter(_ == t)

  /**
    * Matcher which tries to match the input t according to m.
    * If it's a match, then the match will be returned.
    * If it's a miss, then we return a match based on the original t.
    *
    * @param m a Mather[T, T]
    * @tparam T both the type of the input and the underlying type of the output.
    * @return a Matcher[T, T]
    */
  def alt[T](m: Matcher[T, T]): Matcher[T, T] = Matcher {
    t =>
      m(t) match {
        case z@Match(_) => z
        case Miss(_, _) => Match(t)
        case Error(x) => Error(x)
        case _ => throw MatcherException("this case not possible")
      }
  }

  /**
    * Matcher whose success depends on the application of a function f to the input,
    * then the application of a predicate to a control value and the result of f.
    *
    * CONSIDER what to do with this
    *
    * @param f a T => R.
    * @param p a predicate based on the tuple (q, r) where r is the result of applying f to t.
    * @tparam Q the "control" type.
    * @tparam T the "input" type.
    * @tparam R the result type.
    * @return a Matcher[(Q,R), T].
    */
  def valve[Q, T, R](f: T => R, p: (Q, R) => Boolean)(implicit logger: MatchLogger): Matcher[(Q, T), R] = Matcher[(Q, T), R] {
    // CONSIDER redesign this in terms of other Matchers, not MatchResult
    case (q, t) => MatchResult(f, p)(q, t)
  } :| "valve"

  /**
    * Matcher whose success depends on the application of a function f to the input,
    * then the application of a predicate to a control value and the result of f.
    *
    * CONSIDER what to do with this
    *
    * @param p a predicate based on the tuple (q, r) where r is the result of applying f to t.
    * @tparam Q the "control" type.
    * @tparam T the "input" type.
    * @return a Matcher[(Q,R), T].
    */
  def valve[Q, T](p: (Q, T) => Boolean): Matcher[(Q, T), T] = {
    case (q, t) => MatchResult.create(p)(q, t, t)
  }

  /**
    * Method to yield a parser af String from a regular expression.
    *
    * @param regex a Regex.
    * @return a Parser[String]
    */
  def parser(regex: Regex): Parser[String] = doParse(regex, _)

  /**
    * Method to yield a parser af String from a regular expression.
    *
    * @param regex the String representation of the regular expression.
    * @return a Parser[String]
    */
  def parser(regex: String): Parser[String] = parser(regex.r)

  /**
    * @return a Parser[Int]
    */
  lazy val parserInt: Parser[Int] = wholeNumber map (_.toInt)

  /**
    * @return a Parser[Long]
    */
  lazy val parserLong: Parser[Long] = wholeNumber ^^ (_.toLong)

  /**
    * @return a Parser[BigInt]
    */
  lazy val parserBigInt: Parser[BigInt] = wholeNumber ^^ (BigInt(_))

  /**
    * @return a Parser[Double]
    */
  lazy val parserDouble: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  /**
    * @return a Parser[BigDecimal]
    */
  lazy val parserBigDecimal: Parser[BigDecimal] = floatingPointNumber ^^ (BigDecimal(_))

  /**
    * Method to yield a parser af R from a regular expression.
    *
    * @param regex a Regex.
    * @param p     a Parser[R].
    * @tparam R the underlying type of resulting Parser.
    * @return a Parser[R]
    */
  def parseWithParser[R](regex: Regex)(p: Parser[R]): Parser[R] = doParseWithParser(regex, "parseWithParser")(p)

  /**
    * Method to yield a parser af R from a String interpreted as a regular expression.
    *
    * @param regex the String representation of the regular expression.
    * @param p     a Parser[R].
    * @tparam R the underlying type of resulting Parser.
    * @return a Parser[R]
    */
  def parseWithParser[R](regex: String)(p: Parser[R]): Parser[R] = parseWithParser(regex.r)(p)

  /**
    * Method to yield a parser af R from a regular expression and groups.
    * There must be only one matching group.
    *
    * @param regexGroups an instance of RegexGroups.
    * @param p           a Parser[R].
    * @tparam R the underlying type of resulting Parser.
    * @return a Parser[R]
    */
  def parserGroup[R](regexGroups: RegexGroups)(p: Parser[R]): Parser[R] = doParseGroupsWithFunction(regexGroups, "parserGroup") {
    case List(x) => p(x)
    case _ => throw MatcherException("parserGroup: no match")
  }

  /**
    * Method to yield a parser af R from a regular expression and groups.
    * There must be only one matching group.
    *
    * @param regex a Regex.
    * @param group an optional group index, starting at 1 (if empty, then all groups are selected).
    * @param p     a Parser[R].
    * @tparam R the underlying type of resulting Parser.
    * @return a Parser[R]
    */
  def parserGroup[R](regex: Regex, group: Int*)(p: Parser[R]): Parser[R] = parserGroup(RegexGroups(regex, group))(p)

  /**
    * Method to yield a parser af R from a regular expression and groups.
    * There must be only one matching group.
    *
    * @param regex the String representation of the regular expression.
    * @param group an optional group index, starting at 1 (if empty, then all groups are selected).
    * @param p     a Parser[R].
    * @tparam R the underlying type of resulting Parser.
    * @return a Parser[R]
    */
  def parserGroup[R](regex: String, group: Int*)(p: Parser[R]): Parser[R] = parserGroup(regex.r, group: _*)(p)

  /**
    * Method to parse two Strings from a regular expression.
    *
    * @param regexGroups an instance of RegexGroups.
    * @return a Parser[String ~ String]
    */
  def parserTilde(regexGroups: RegexGroups): Parser[String ~ String] = doParseGroupsWithFunction(regexGroups, "parserTilde") {
    case List(x, y) => Match(x) ~ Match(y)
    case _ => throw MatcherException("parserTilde: no match")
  }

  /**
    * Method to parse two Strings from a regular expression.
    *
    * @param regex  a Regex.
    * @param groups a list of required group indexes, starting at 1 (if empty, then all groups are selected).
    * @return a Parser[String ~ String]
    */
  def parserTilde(regex: Regex, groups: Int*): Parser[String ~ String] = parserTilde(RegexGroups.create(regex, groups: _*))

  /**
    * Method to parse two Strings from a regular expression.
    *
    * @param regex  the String representation of the regular expression.
    * @param groups a list of required group indexes, starting at 1 (if empty, then all groups are selected).
    * @return a Parser[String ~ String]
    */
  def parserTilde(regex: String, groups: Int*): Parser[String ~ String] = parserTilde(regex.r, groups: _*)

  /**
    * Parser which succeeds if the input matches the given regular expression.
    * The result of the parser is the desired List of matched Strings.
    *
    * @param rg an instance of RegexGroups.
    * @return a Parser[List of Strings].
    */
  def parserList(rg: RegexGroups): Parser[List[String]] = doParseGroups(rg, _)

  /**
    * Parser which succeeds if the input matches the given regular expression.
    * The result of the parser is the desired List of matched Strings.
    *
    * @param regex  a regular expression.
    * @param groups a list of required group indexes, starting at 1 (if empty, then all groups are selected).
    * @return a Parser[List of Strings].
    */
  def parserList(regex: Regex, groups: Int*): Parser[List[String]] = parserList(RegexGroups(regex, groups))

  /**
    * Parser which succeeds if the input matches the given regular expression.
    * The result of the parser is the desired List of matched Strings.
    *
    * @param regex  a regular expression as a String.
    * @param groups a list of required group indexes, starting at 1 (if empty, then all groups are selected).
    * @return a Parser[List of Strings].
    */
  def parserList(regex: String, groups: Int*): Parser[List[String]] = parserList(regex.r, groups: _*)

  /**
    * Method to yield a Parser[Z] using the given regex, a function String=>P0 and a function P0=>Z.
    * Typically, Z will be a case class.
    *
    * TESTME
    *
    * @param regex     a regular expression.
    * @param groups    a list of required group indexes, starting at 1 (if empty, then all groups are selected).
    * @param p0        a Parser[P0].
    * @param construct a function P0 => Z.
    * @tparam P0 the type of the first member of Z.
    * @tparam Z  the underlying type of the resulting parser.
    * @return a Parser[Z].
    */
  def parser1[P0, Z](regex: String, groups: Int*)(p0: Parser[P0])(construct: P0 => Z): Parser[Z] =
    doParseGroupsWithFunction(RegexGroups.create(regex, groups: _*), "parser1") {
      case List(x) => p0(x) map construct
      case _ => throw MatcherException("parser1: no match")
    }

  /**
    * Method to yield a Parser[Z] using the given regex, several intermediate functions, and a function (P0, P1)=>Z.
    * Typically, Z will be a case class with two members.
    *
    * @param regex     a regular expression.
    * @param groups    a list of required group indexes, starting at 1 (if empty, then all groups are selected).
    * @param p0        a Parser[P0].
    * @param p1        a Parser[P1].
    * @param construct a function (P0, P1) => Z.
    * @tparam P0 the type of the first member of Z.
    * @tparam P1 the type of the second member of Z.
    * @tparam Z  the underlying type of the resulting parser.
    * @return a Parser[Z].
    */
  def parser2[P0, P1, Z](regex: String, groups: Int*)(p0: Parser[P0], p1: Parser[P1])(construct: (P0, P1) => Z): Parser[Z] =
    doParseGroupsWithFunction(RegexGroups.create(regex, groups: _*), "parser2") {
      case List(x, y) => p0(x) ~ p1(y) map {
        case u ~ v => construct(u, v)
      }
      case _ => throw MatcherException("parser2: no match")
    }

  /**
    * Method to yield a Parser[Z] using the given regex, several intermediate functions, and a function (P0, P1, P2)=>Z.
    * Typically, Z will be a case class with three members.
    *
    * @param regex     a regular expression.
    * @param groups    a list of required group indexes, starting at 1 (if empty, then all groups are selected).
    * @param p0        a Parser[P0].
    * @param p1        a Parser[P1].
    * @param p2        a Parser[P2].
    * @param construct a function (P0, P1, P2) => Z.
    * @tparam P0 the type of the first member of Z.
    * @tparam P1 the type of the second member of Z.
    * @tparam P2 the type of the third member of Z.
    * @tparam Z  the underlying type of the resulting parser.
    * @return a Parser[Z].
    */
  def parser3[P0, P1, P2, Z](regex: String, groups: Int*)(p0: Parser[P0], p1: Parser[P1], p2: Parser[P2])(construct: (P0, P1, P2) => Z): Parser[Z] =
    doParseGroupsWithFunction(RegexGroups.create(regex, groups: _*), "parser3") {
      case List(x, y, z) => p0(x) ~ p1(y) ~ p2(z) map {
        case u ~ v ~ w => construct(u, v, w)
      }
      case _ => throw MatcherException("parser3: no match")
    }

  /**
    * Matcher which reverses the sense of this Matcher.
    * However, an Error remains an Error.
    *
    * @param m a Matcher[T, R]
    * @param r the default result value, only to be used in the even of a Miss.
    * @tparam T the input type of m.
    * @tparam R the result type of m.
    * @return a Matcher[T, R] which works in the opposite sense to this.
    */
  def not[T, R](m: Matcher[T, R], r: => R): Matcher[T, R] = t => m(t) match {
    case Match(_) => Miss("not", t)
    case Miss(_, _) => Match(r)
    case Error(e) => Error(e)
    case x => throw MatcherException(s"not: logic error: $x")
  }

  /**
    * Matcher which always succeeds but whose result is an Option[R].
    * If the bound variable t of the resulting Matcher is null, then it will result in a Match(None).
    *
    * @param m a Matcher[T, R]
    * @tparam T the input type of m.
    * @tparam R the result type of m.
    * @return Matcher[T, Option of R]
    */
  def opt[T, R](m: Matcher[T, R]): Matcher[T, Option[R]] = Matcher(t => sequence(Option(t) map m))

  /**
    * Method to match a T, resulting in an R, where the match is indirectly determined by
    * the given lens function.
    *
    * @param m    a Matcher[U, R].
    * @param lens a function T => U.
    * @tparam U the type of a property that is matched by m.
    * @return a Matcher[T, R]
    */
  def having[T, U, R](m: Matcher[U, R])(lens: T => U): Matcher[T, R] = t => m(lens(t))

  /**
    * Method to create a Matcher which operates on a similar, but inverted, ~ as m.
    *
    * @param m a Matcher[T0 ~ T1, R].
    * @tparam T0 one of the input types.
    * @tparam T1 the other input type.
    * @tparam R  the result type.
    * @return a Matcher[T1 ~ T0, R].
    */
  def flip[T0, T1, R](m: Matcher[T0 ~ T1, R]): Matcher[T1 ~ T0, R] = Matcher {
    case t1 ~ t0 => m(t0 ~ t1)
  }

  /**
    * Matcher which tries m on the given (~) input.
    * If m is unsuccessful, it then tries m on the swapped (inverted) ~.
    *
    * @param m        a Matcher[T ~ T, R].
    * @param commutes if true (the default), the order of the incoming ~ elements is immaterial,
    *                 thus we can try flipping their order.
    *                 If false then we do not try.
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T ~ T, R].
    */
  def *[T, R](m: Matcher[T ~ T, R], commutes: Boolean = true): Matcher[T ~ T, R] = m | (maybe[T ~ T](commutes) & swap & m)

  /**
    * Matcher which tries m on the given (~~) input.
    * If m is unsuccessful, it then tries m on the rotated ~~.
    * If that's unsuccessful, it then tries m on the inverted ~~.
    *
    * @param m        a Matcher[T ~ T ~ T, R].
    * @param commutes if true (the default), the order of the incoming ~ elements is immaterial,
    *                 thus we can try rotating their order.
    *                 If false then we do not try.
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T ~ T ~ T, R].
    */
  def **[T, R](m: Matcher[T ~ T ~ T, R], commutes: Boolean = true): Matcher[T ~ T ~ T, R] =
    m | (maybe[T ~ T ~ T](commutes) & rotate3 & m) | (maybe[T ~ T ~ T](commutes) & invert3 & m)

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a T0, based on the first element of P.
    *
    * @param f method to convert a (T0, T1) into a P (used at compile-time only).
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,T0]
    */
  def select2_0[T0, T1, P <: Product](f: (T0, T1) => P): Matcher[P, T0] = lift(
    p => p.productElement(0).asInstanceOf[T0]
  )

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a T1, based on the second element of P.
    *
    * @param f method to convert a (T0, T1) into a P (used at compile-time only).
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,T1]
    */
  def select2_1[T0, T1, P <: Product](f: (T0, T1) => P): Matcher[P, T1] = lift(
    p => p.productElement(1).asInstanceOf[T1]
  )

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a T0, based on the first element of P.
    *
    * @param f method to convert a (T0, T1, T2) into a P (used at compile-time only).
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,T0]
    */
  def select3_0[T0, T1, T2, P <: Product](f: (T0, T1, T2) => P): Matcher[P, T0] = lift(
    p => p.productElement(0).asInstanceOf[T0]
  )

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a T1, based on the second element of P.
    *
    * @param f method to convert a (T0, T1, T2) into a P (used at compile-time only).
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,T1]
    */
  def select3_1[T0, T1, T2, P <: Product](f: (T0, T1, T2) => P): Matcher[P, T1] = lift(
    p => p.productElement(1).asInstanceOf[T1]
  )

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a T2, based on the third element of P.
    *
    * @param f method to convert a (T0, T1, T2) into a P (used at compile-time only).
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,T1]
    */
  def select3_2[T0, T1, T2, P <: Product](f: (T0, T1, T2) => P): Matcher[P, T2] = lift(
    p => p.productElement(2).asInstanceOf[T2]
  )

  /**
    * Method to create a Matcher of a Tilde, which succeeds if the first member matches the given Matcher.
    *
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam R0 type of result's first member.
    * @return a Matcher[T0 ~ T1, R0 ~ T1].
    */
  def filter2_0[T0, T1, R0](m: Matcher[T0, R0]): Matcher[T0 ~ T1, R0 ~ T1] = {
    case t0 ~ t1 => m(t0) ~ Match(t1)
  }

  /**
    * Method to create a Matcher of ~, which succeeds if the first member matches the given Matcher.
    *
    * @tparam T0 first of the input member types.
    * @tparam T1 second of the input member types.
    * @tparam R1 type of result's second member.
    * @return a Matcher[T0 ~ T1, T0 ~ R1].
    */
  def filter2_1[T0, T1, R1](m: Matcher[T1, R1]): Matcher[T0 ~ T1, T0 ~ R1] = {
    case t0 ~ t1 => Match(t0) ~ m(t1)
  }

  /**
    * Method to create a Matcher of ~~, which succeeds if the first member matches the given Matcher.
    *
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam R0 type of result's first member.
    * @return a Matcher[T0 ~ T1 ~ T2, R0 ~ T1 ~ T2].
    */
  def filter3_0[T0, T1, T2, R0](m: Matcher[T0, R0]): Matcher[T0 ~ T1 ~ T2, R0 ~ T1 ~ T2] = {
    case t0 ~ t1 ~ t2 => m(t0) ~ Match(t1) ~ Match(t2)
  }

  /**
    * Method to create a Matcher of ~~, which succeeds if the second member matches the given Matcher.
    *
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam R1 type of result's second member.
    * @return a Matcher[T0 ~ T1 ~ T2, T0 ~ R1 ~ T2].
    */
  def filter3_1[T0, T1, T2, R1](m: Matcher[T1, R1]): Matcher[T0 ~ T1 ~ T2, T0 ~ R1 ~ T2] = {
    case t0 ~ t1 ~ t2 => Match(t0) ~ m(t1) ~ Match(t2)
  }

  /**
    * Method to create a Matcher of ~~, which succeeds if the third member matches the given Matcher.
    *
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam R2 type of result's third member.
    * @return a Matcher[T0 ~ T1 ~ T2, T0 ~ T1 ~ R2].
    */
  def filter3_2[T0, T1, T2, R2](m: Matcher[T2, R2]): Matcher[T0 ~ T1 ~ T2, T0 ~ T1 ~ R2] = {
    case t0 ~ t1 ~ t2 => Match(t0) ~ Match(t1) ~ m(t2)
  }

  /**
    * Method to swap the order of elements in a Tilde.
    *
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @return a Matcher from T0 ~ T1 to T1 ~ T0.
    */
  def swap[T0, T1]: Matcher[T0 ~ T1, T1 ~ T0] = lift[T0 ~ T1, T1 ~ T0] {
    case t0 ~ t1 => t1 ~ t0
  }

  /**
    * Method to rotate the order of elements in a ~~.
    *
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @tparam T2 the third element type.
    * @return a Matcher[T0 ~ T1 ~ T2, T1 ~ T2 ~ T0].
    */
  def rotate3[T0, T1, T2]: Matcher[T0 ~ T1 ~ T2, T1 ~ T2 ~ T0] = lift {
    case t0 ~ t1 ~ t2 => t1 ~ t2 ~ t0
  }

  /**
    * Method to invert the order of elements in a ~~.
    *
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @tparam T2 the third element type.
    * @return a Matcher[T0 ~ T1 ~ T2, T2 ~ T1 ~ T0].
    */
  def invert3[T0, T1, T2]: Matcher[T0 ~ T1 ~ T2, T2 ~ T1 ~ T0] = lift {
    case t0 ~ t1 ~ t2 => t2 ~ t1 ~ t0
  }

  /**
    * (Internal) log method.
    * If ll is LogOff, p is returned unchanged, other than that on failure of m, the matcher fail(name) is invoked.
    * If ll is LogInfo, a matcher based on m, which on successful matching, logging with println will occur, is returned.
    * If ll is LogDebug, then the value of log(m)(name) is returned.
    *
    * @param m  a Matcher[T, R].
    * @tparam T the underlying type of the input to m.
    * @tparam R the underlying type of the result of m.
    * @return a Matcher[T, R].
    */
  def log[T, R](m: => Matcher[T, R])(implicit logger: MatchLogger): Matcher[T, R] = logger.logLevel match {
    case LogDebug => constructMatcher[T, R] {
      t =>
        logger(s"trying matcher ${m.toString} on $t...")
        val r: MatchResult[R] = m(t)
        logger(s"... ${m.toString}: $r")
        r
    }

    case LogInfo => Matcher(t => m(t) :- (x => logger(s"${m.toString}: matched $x")))

    case _ => m
  }

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a ~.
    *
    * @param f method to convert a (T0, T1) into a P.
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P, T0 ~ T1].
    */
  def tilde2[T0, T1, P <: Product](f: (T0, T1) => P): Matcher[P, T0 ~ T1] = lift(
    p => p.productElement(0).asInstanceOf[T0] ~ p.productElement(1).asInstanceOf[T1]
  )

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a ~~.
    *
    * @param f method to convert a (T0, T1, T2) into a P.
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P, T0 ~ T1 ~ T2].
    */
  def tilde3[T0, T1, T2, P <: Product](f: (T0, T1, T2) => P): Matcher[P, T0 ~ T1 ~ T2] = lift(
    p => p.productElement(0).asInstanceOf[T0] ~ p.productElement(1).asInstanceOf[T1] ~ p.productElement(2).asInstanceOf[T2]
  )

  /**
    * Method to create a Matcher, which always succeeds, of a ~ whose result is a P.
    * This method is the inverse of tilde2.
    *
    * @param f method to convert a (T0, T1) into a P.
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam P  the product type.
    * @return a Matcher[T0 ~ T1, P].
    */
  def product2[T0, T1, P <: Product](f: (T0, T1) => P): Matcher[T0 ~ T1, P] = lift {
    case x ~ y => f(x, y)
  }

  /**
    * Method to create a Matcher, which always succeeds, of a ~~ whose result is a P.
    * This method is the inverse of tilde2.
    *
    * @param f method to convert a (T0, T1) into a P.
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam P  the product type.
    * @return a Matcher[T0 ~ T1 ~ T2, P].
    */
  def product3[T0, T1, T2, P <: Product](f: (T0, T1, T2) => P): Matcher[T0 ~ T1 ~ T2, P] = lift {
    case x ~ y ~ z => f(x, y, z)
  }

  /**
    * Method to match any element of a ~.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam R  the MatchResult type.
    * @return a Matcher[T0 ~ T1, R] that matches at least one of the elements of the given tilde.
    */
  def match2Any[T0, T1, R](m0: Matcher[T0, R], m1: => Matcher[T1, R]): Matcher[T0 ~ T1, R] = {
    case t0 ~ t1 => matchProduct2Any(m0, m1)(->.apply)(t0 -> t1)
  }

  /**
    * Method to match any element of a Product with two elements.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @param f  a function which takes a (t0, t1) and returns a P.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam R  the MatchResult type.
    * @tparam P  the input type.
    * @return a Matcher[P, R] that matches at least one of the elements of the given P.
    */
  def matchProduct2Any[T0, T1, R, P <: Product](m0: Matcher[T0, R], m1: => Matcher[T1, R])(f: (T0, T1) => P): Matcher[P, R] = p =>
    m0(p.productElement(0).asInstanceOf[T0]) || m1(p.productElement(1).asInstanceOf[T1])

  /**
    * Method to match any element of a Tuple3.
    *
    * TODO fix me
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @param m2 the Matcher corresponding to the third element.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam T2 the input type for the second Matcher.
    * @tparam R  the MatchResult type.
    * @return a Matcher[(T0, T1, T2), R] that matches at least one of the elements of the given tuple.
    */
  def match3Any[T0, T1, T2, R](m0: Matcher[T0, R], m1: => Matcher[T1, R], m2: => Matcher[T2, R]): Matcher[(T0, T1, T2), R] = {
    case (t0, t1, t2) =>
      val f: (T0, T1, T2) => (T0, T1, T2) = (t0, t1, t2) => Tuple3(t0, t1, t2)
      matchProduct3Any(m0, m1, m2)(f)(t0, t1, t2)
  }

  /**
    * Method to match any element of a Product with two elements.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @param m2 the Matcher corresponding to the third element.
    * @param f  a function which takes a (t0, t1, t3) and returns a P.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam T2 the input type for the third Matcher.
    * @tparam R  the MatchResult type.
    * @tparam P  the input type.
    * @return a Matcher[P, R] that matches at least one of the elements of the given P.
    */
  def matchProduct3Any[T0, T1, T2, R, P <: Product](m0: Matcher[T0, R], m1: => Matcher[T1, R], m2: => Matcher[T2, R])(f: (T0, T1, T2) => P): Matcher[P, R] = p =>
    m0(p.productElement(0).asInstanceOf[T0]) || m1(p.productElement(1).asInstanceOf[T1]) || m2(p.productElement(2).asInstanceOf[T2])

  /**
    * Method to match any element of a Product with two elements.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @param f  a function which takes a (t0, t1) and returns a P.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam R0 the first MatchResult type.
    * @tparam R1 the second MatchResult type.
    * @tparam P  the input type.
    * @return a Matcher[P, R0 ~ R1] that matches at least one of the elements of the given P.
    */
  def matchProduct2All[T0, T1, R0, R1, P <: Product](m0: Matcher[T0, R0], m1: => Matcher[T1, R1])(f: (T0, T1) => P): Matcher[P, R0 ~ R1] = p =>
    m0(p.productElement(0).asInstanceOf[T0]) ~ m1(p.productElement(1).asInstanceOf[T1])

  /**
    * Method to match any element of a Product with two elements.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @param m2 the Matcher corresponding to the third element.
    * @param f  a function which takes a (t0, t1, t3) and returns a P.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam T2 the input type for the third Matcher.
    * @tparam R0 the first MatchResult type.
    * @tparam R1 the second MatchResult type.
    * @tparam R2 the third MatchResult type.
    * @tparam P  the input type.
    * @return a Matcher[P, R0 ~ R1 ~ R2] that matches at least one of the elements of the given P.
    */
  def matchProduct3All[T0, T1, T2, R0, R1, R2, P <: Product](m0: Matcher[T0, R0], m1: => Matcher[T1, R1], m2: => Matcher[T2, R2])(f: (T0, T1, T2) => P): Matcher[P, R0 ~ R1 ~ R2] = p =>
    m0(p.productElement(0).asInstanceOf[T0]) ~ m1(p.productElement(1).asInstanceOf[T1]) ~ m2(p.productElement(2).asInstanceOf[T2])

  /**
    * Method to match all elements of a ~.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam R0 the MatchResult type for m0.
    * @tparam R1 the MatchResult type for m1.
    * @return a Matcher[T0 ~ T1, R0 ~ R1] that matches all the elements of the given ~.
    */
  def match2All[T0, T1, R0, R1](m0: Matcher[T0, R0], m1: => Matcher[T1, R1]): Matcher[T0 ~ T1, R0 ~ R1] = {
    case t0 ~ t1 => m0(t0) ~ m1(t1)
  }

  /**
    * Method to match all elements of a ~~.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam R0 the MatchResult type for m0.
    * @tparam R1 the MatchResult type for m1.
    * @tparam R2 the MatchResult type for m2.
    * @return a Matcher[T0 ~ T1 ~ T2, R0 ~ R1 ~ R2] that matches at least one of the elements of the given ~~.
    */
  def match3All[T0, T1, T2, R0, R1, R2](m0: Matcher[T0, R0], m1: => Matcher[T1, R1], m2: => Matcher[T2, R2]): Matcher[T0 ~ T1 ~ T2, R0 ~ R1 ~ R2] = {
    case t0 ~ t1 ~ t2 => m0(t0) ~ m1(t1) ~ m2(t2)
  }

  /**
    * Implicit class MatcherOps which allows us to use the method :| on a Matcher[T,R].
    *
    * @param p a Matcher[T,R].
    * @tparam T the input type of p.
    * @tparam R the result type of p.
    */
  implicit class MatcherOps[T, R](p: Matcher[T, R]) {
    def :|(name: => String)(implicit logger: MatchLogger): Matcher[T, R] = log(p.named(name))
  }

  implicit class TildeOps[R, S](r: R) {
    def ~(s: S): R ~ S = new ~(r, s)
  }

  /**
    * Implicit class MatchResultOps which allows us to use the method tee on a MatchResult[R].
    *
    * @param rr a MatchResult[R].
    * @tparam R the result type of m.
    */
  implicit class MatchResultOps[R](rr: MatchResult[R]) {
    /**
      * This method can be pronounced as "tee" as it's like a tee in a pipe.
      * The input and the output are identical (so it's like identity) but it has a side-effect:
      * the function f is invoked on the matched value of r (assuming that it is a Match).
      *
      * @param f a R => Unit function.
      * @return rr unchanged.
      */
    def :-(f: R => Unit): MatchResult[R] = {
      rr foreach f
      rr
    }
  }

  /**
    * Implicit class ParserOps which allows us to use the method m on a String.
    *
    * @param s a String.
    */
  implicit class ParserOps(s: String) {
    /**
      * Method to create a Matcher which simply matches the String s.
      * Basically a convenience to turn a String into a literal String matcher.
      *
      * @return Parser[String]
      */
    def m: Parser[String] = matches(s)

    /**
      * Method to create a Parser which matches the String s interpreted as a regular expression.
      * Basically a convenience to avoid having to depend on Scala Parser Combinators for simple
      * regular expressions.
      *
      * @return Parser[String]
      */
    def regex: Parser[String] = parser(s)

    /**
      * Method to create a Parser which matches the String s interpreted as a regular expression.
      * Basically a convenience to avoid having to depend on Scala Parser Combinators for simple
      * regular expressions.
      * Use this method rather than regex if you need to process the matched groups individually.
      *
      * @return Parser[List of Strings]
      */
    def regexGroups: Parser[List[String]] = Try(RegexGroups(s.r())) map parserList match {
      case Success(m) => m
      case Failure(x) => _ => Error(x)
    }
  }

  /**
    * Trait to define the behavior of the result of a Match.
    *
    * @tparam R the type of the result.
    */
  sealed trait MatchResult[+R] {
    /**
      * @return true if this is a Match
      */
    def successful: Boolean

    /**
      * @return the result of the MatchResult.
      * @throws Throwable (a MatcherException) if this is not a Match.
      */
    def get: R

    /**
      * "unit" method for a successful match.
      *
      * @param s the call-by-name value of the result.
      * @tparam S the underlying type of the result.
      * @return a Match[S] with value s (unless s throws an exception, in which case the result will be an Error).
      */
    def success[S](s: => S): MatchResult[S] = MatchResult(s)

    /**
      * FlatMap method.
      * If this is a Match(r), then return f(r).
      * Otherwise, we return an unsuccessful result based on this.
      *
      * @param f a function of R => MatchResult[S].
      * @tparam S the underlying type of the returned MatchResult.
      * @return MatchResult[S].
      */
    def flatMap[S](f: R => MatchResult[S]): MatchResult[S]

    /**
      * Alternative form of get such that, in the case of a Miss, the default value given by s will be returned.
      *
      * @param s a call-by-name value to be used if this is unsuccessful.
      * @tparam S a super-class of R.
      * @return the result of the MatchResult if it's a Match, otherwise return s if it's a Miss.
      */
    def getOrElse[S >: R](s: => S): S

    /**
      * Method to compose this MatchResult with sm.
      * The results of this and of sm are combined into one ~.
      * The ~ method is a synonym of andThen.
      *
      * @param sm a call-by-name MatchResult[S].
      * @tparam S the underlying type of s.
      * @return a MatchResult[R ~ S].
      */
    def andThen[S](sm: => MatchResult[S]): MatchResult[R ~ S]

    /**
      * Alternation method which takes a MatchResult as the alternative.
      *
      * @param sm a call-by-name MatchResult which will be used if this is empty.
      * @tparam S the type of the result and a super-type of R.
      * @return a MatchResult[S], either this (if successful) otherwise sm.
      */
    def orElse[S >: R](sm: => MatchResult[S]): MatchResult[S]

    /**
      * Foreach method.
      *
      * @param f a function of R => Unit.
      * @return Unit.
      */
    def foreach(f: R => Unit): Unit

    /**
      * Method to compose this MatchResult with sm.
      * It this is successful, then sm will be returned.
      * Otherwise, an Unsuccessful result will be returned.
      *
      * Similar to ~ except that this MatchResult[R] is discarded and not part of the result.
      * In other words, this acts purely as a guard.
      * Synonym: &&.
      *
      * @param sm the MatchResult which must follow this MatchResult for a successful outcome.
      * @tparam S the type of the resulting MatchResult.
      * @return a MatchResult[S].
      */
    def guard[S](sm: => MatchResult[S]): MatchResult[S]

    /**
      * Alternation method which takes a Matcher as the alternative.
      * If this MatchResult is empty then return the value of m applied to the input.
      *
      * @param m a Matcher of Any to S.
      * @tparam S the type of the result and a super-type of R.
      * @return a MatchResult[S].
      */
    def |[S >: R](m: => Matcher[Any, S]): MatchResult[S]

    /**
      * Composition method.
      * If this MatchResult is successful then return the value of m applied to the result.
      *
      * @param m a call-by-name Matcher of S to T.
      * @tparam S the underlying type of the input to m (S is a super-class of R).
      * @tparam T the underlying type of the returned value.
      * @return a MatchResult[T].
      */
    def &[S >: R, T](m: => Matcher[S, T]): MatchResult[T]

    /**
      * Map method.
      *
      * @param f a function of R => S.
      * @tparam S the underlying type of the returned MatchResult.
      * @return MatchResult[S].
      */
    def map[S](f: R => S): MatchResult[S] = flatMap(r => success(f(r)))

    /**
      * Method to determine if this is unsuccessful.
      *
      * @return the negation of successful.
      */
    def isEmpty: Boolean = !successful

    /**
      * Method to compose this MatchResult with sm.
      * The results of this and of sm are combined into one ~.
      * ~ is a synonym of andThen.
      *
      * @param sm a call-by-name MatchResult[S].
      * @tparam S the underlying type of s.
      * @return a MatchResult[R ~ S].
      */
    def ~[S](sm: => MatchResult[S]): MatchResult[R ~ S] = andThen(sm)

    /**
      * Alternation method which takes a MatchResult as the alternative.
      * Identical to orElse.
      *
      * @param sm a call-by-name MatchResult which will be used if this is empty.
      * @tparam S the type of the result and a super-type of R.
      * @return a MatchResult[S] which is the result of invoking orElse(sm).
      */
    def ||[S >: R](sm: => MatchResult[S]): MatchResult[S] = orElse(sm)

    /**
      * Method to compose this MatchResult with sm.
      * It this is successful, then sm will be returned.
      * Otherwise, an Unsuccessful result will be returned.
      *
      * Similar to ~ except that this MatchResult[R] is discarded and not part of the result.
      * In other words, this acts purely as a guard.
      * This method simply invokes guard(sm).
      *
      * @param sm the MatchResult which must follow this MatchResult for a successful outcome.
      * @tparam S the type of the resulting MatchResult.
      * @return a MatchResult[S] which is the result of calling guard(sm).
      */
    def &&[S](sm: => MatchResult[S]): MatchResult[S] = guard(sm)
  }

  /**
    * Trait Matcher.
    *
    * @tparam T the input type.
    * @tparam R the result type.
    */
  trait Matcher[-T, +R] extends (T => MatchResult[R]) {

    /**
      * Unit method which yields a Matcher
      *
      * @param s the instance of type S which will be wrapped in Match for the result of the resulting Matcher.
      * @tparam S the result type for the resulting Matcher.
      * @return a Matcher[T, S].
      */
    def unit[S](s: S): Matcher[T, S] = _ => Match(s)

    /**
      * Map method which transforms this Matcher[T, R] into a Matcher[T, S].
      *
      * @param f a function R => S.
      * @tparam U the input type for the resulting Matcher (U is a subtype of T).
      * @tparam S the result type for the resulting Matcher.
      * @return a Matcher[U, S].
      */
    def map[U <: T, S](f: R => S): Matcher[U, S] = this (_) map f

    /**
      * Map method which transforms this Matcher[T, R] into a Matcher[U, S].
      *
      * @param f a function R => S.
      * @tparam U the input type for the resulting Matcher (U is a subtype of T).
      * @tparam S the result type for the resulting Matcher.
      * @return a Matcher[T, S].
      */
    def flatMap[U <: T, S](f: R => MatchResult[S]): Matcher[U, S] = this (_) flatMap f

    /**
      * Method to transform a MatchResult.
      *
      * `p ^^ f` succeeds if `p` succeeds; it returns `f` applied to the result of `p`.
      *
      * @param f a function that will be applied to this matcher's result (see `map` in `MatchResult`).
      * @return a parser that has the same behaviour as the current matcher, but whose result is
      *         transformed by `f`.
      */
    def ^^[S](f: R => S): Matcher[T, S] = map(f).named(toString + "^^")

    /**
      * Matcher which reverses the sense of this Matcher.
      *
      * @param s the default result value, only to be used in the even of a Miss.
      * @tparam S the type of both s and the result (a super-type of R).
      * @return a Matcher[T, R] which works in the opposite sense to this.
      */
    def ![S >: R](s: => S): Matcher[T, S] = not(this, s)

    /**
      * Returns a matcher that optionally matches what this parser parses.
      *
      * @return opt(this)
      */
    def ? : Matcher[T, Option[R]] = opt(this)

    /**
      * Method to combine Matchers in the sense that, if this fails, then we try to match using m.
      *
      * NOTE: changed the logic to avoid using match2Any because it is unnecessarily convoluted.
      *
      * NOTE: if equals is not properly implemented for type T, it is possible to get into an infinite recursion.
      * This is actually Issue #14.
      *
      * @param m the alternative Matcher.
      * @return a Matcher[T, R] which will match either on this or on m.
      */
    def |[U <: T, S >: R](m: Matcher[U, S]): Matcher[U, S] = t =>
      this (t) match {
        case x@Match(_) => x
        case _ => m(t)
      }

    /**
      * Method to combine Matchers in the sense that, when this successfully matches a T, resulting in an R,
      * then m is invoked on the result, such that if it is successful, we return an S..
      *
      * @param m the alternative Matcher.
      * @tparam S the underlying type of the resulting Matcher.
      * @return a Matcher[T, S] which will match in composition on both this and m.
      */
    def &[S](m: Matcher[R, S]): Matcher[T, S] = t => this (t) & m

    /**
      * Method to combine Matchers this and m such that the resulting Matcher takes a ~ and results in a ~.
      *
      * @param m a Matcher[P, S]
      * @tparam P the input type of m.
      * @tparam S the result type of m.
      * @return a Matcher[T ~ P, R ~ S] which is the result of invoking match2All(this, m).
      */
    def ~[P, S](m: Matcher[P, S]): Matcher[T ~ P, R ~ S] = match2All(this, m)

    /**
      * Method to combine Matchers this and m such that the resulting Matcher takes a ~ and results in the result from m.
      *
      * @param m a Matcher[P, S]
      * @tparam P the input type of m.
      * @tparam S the result type of m.
      * @return a Matcher[(T,P), S] which is the result of invoking ~ but stripping the first element of the ~.
      */
    def ~>[P, S](m: Matcher[P, S]): Matcher[T ~ P, S] = this ~ m ^^ { case _ ~ y => y }

    /**
      * Method to combine Matchers this and m such that the resulting Matcher takes a ~ and results in the result from this.
      *
      * @param m a Matcher[P, S]
      * @tparam P the input type of m.
      * @tparam S the result type of m.
      * @return a Matcher[(T,P), R] which is the result of invoking ~ but stripping the second element of the ~.
      */
    def <~[P, S](m: Matcher[P, S]): Matcher[T ~ P, R] = this ~ m ^^ { case x ~ _ => x }

    /**
      * CONSIDER maybe doesn't make sense (compare with valve).
      *
      * Matcher which succeeds or not, depending on an additional Q value (the control).
      *
      * @param m a Matcher[(Q, R),U].
      * @tparam Q the type of the control value.
      * @tparam U the result type of m and the returned Matcher.
      * @return a Matcher[(Q,T),U].
      */
    def chain[Q, U](m: Matcher[(Q, R), U]): Matcher[(Q, T), U] = {
      case (q, t) => this (t) flatMap (r => m(q, r))
    }

    /**
      * Matcher which always succeeds (unless this causes an Error) but whose result is based on a Try[R].
      *
      * TESTME more
      *
      * @return Matcher[T, Option of R]
      */
    def trial: Matcher[T, Try[R]] = t =>
      Try(this (t)) match {
        case Success(Match(z)) => Match(Success(z))
        case Success(Miss(x, _)) => Match(Failure(MatcherException(x)))
        case Failure(x) => Match(Failure(x))
        case Success(Error(e)) => Error(e)
        case x => throw MatcherException(s"trial: logic error: $x")
      }

    /**
      * Mutating method which sets the name of this Matcher to n.
      *
      * @param n the new name of this Matcher.
      * @return this.type.
      */
    def named(n: String): this.type = {
      name = n
      this
    }

    override def toString: String = name

    private var name: String = ""
  }

  /**
    * Type alias for Parser.
    */
  type Parser[R] = Matcher[String, R]

  /**
    * Successful match.
    *
    * @param r the result.
    * @tparam R the type of the result.
    */
  case class Match[+R](r: R) extends MatchResult[R] {
    /**
      * @return true
      */
    def successful: Boolean = true

    /**
      * Method to compose this MatchResult with sm.
      * It this is successful, then sm will be returned.
      * Otherwise, an Unsuccessful result will be returned.
      *
      * @param sm the MatchResult which must follow this MatchResult for a successful outcome.
      * @tparam S the type of the resulting MatchResult.
      * @return sm.
      */
    def guard[S](sm: => MatchResult[S]): MatchResult[S] = sm

    /**
      * Alternation method which takes a MatchResult as the alternative.
      *
      * @param sm a MatchResult (ignored)).
      * @return this.
      */
    def orElse[S >: R](sm: => MatchResult[S]): MatchResult[S] = this

    /**
      * If s is a Match, then the result will be a Match of the ~ of r and the result of s.
      *
      * @param sm a MatchResult[S].
      * @tparam S the underlying type of s.
      * @return a MatchResult[(R,S)].
      */
    def andThen[S](sm: => MatchResult[S]): MatchResult[R ~ S] = sm.flatMap(z => Match(r ~ z))

    /**
      * Returns the result of invoking f on r.
      *
      * @param f a function of R => MatchResult[S].
      * @tparam S the underlying type of the returned MatchResult.
      * @return MatchResult[S].
      */
    def flatMap[S](f: R => MatchResult[S]): MatchResult[S] = f(r)

    /**
      * Apply f to r.
      *
      * @param f a function of R => Unit.
      */
    def foreach(f: R => Unit): Unit = f(r)

    /**
      * Alternation method which takes a Matcher as the alternative.
      * If this MatchResult is empty then return the value of m applied to the input.
      *
      * @param m a Matcher of Any to R (ignored).
      * @return this.
      */
    def |[S >: R](m: => Matcher[Any, S]): MatchResult[S] = this

    /**
      * Composition method.
      * If this MatchResult is successful then return the value of m applied to the result.
      *
      * @param m a Matcher of S to T.
      * @tparam S the underlying type of the input to m (S is a super-class of R).
      * @tparam T the underlying type of the returned value.
      * @return m(get).
      */
    def &[S >: R, T](m: => Matcher[S, T]): MatchResult[T] = m(get)

    /**
      * @return r.
      */
    def get: R = r

    /**
      * Alternative form of get such that, in the case of a Miss, the default value given by s will be returned.
      *
      * @param s a call-by-name value to be used if this is unsuccessful.
      * @tparam S a super-class of R.
      * @return r.
      */
    def getOrElse[S >: R](s: => S): S = r

    /**
      * Render this Match as a String.
      *
      * @return s"Match: r"
      */
    override def toString: String = s"Match: $r"
  }

  /**
    * Unsuccessful match of type dependent on X.
    *
    * @param x either a String or a Throwable.
    * @tparam X the type of x.
    * @tparam R the result-type of this.
    */
  abstract class Unsuccessful[+R, X](x: X) extends MatchResult[R] {
    /**
      *
      * @return false.
      */
    def successful: Boolean = false

    /**
      * get method (will always throw an exception).
      *
      * @throws Throwable (a MatcherException) or other Throwable (e).
      */
    def get: R = x match {
      case x: Throwable => throw x
      case w: String => throw MatcherException(s"Unsuccessful: cannot call get: $w")
      case x => throw MatcherException(s"Unsuccessful: cannot call get: $x")
    }

    /**
      * Do nothing.
      *
      * @param f a function of R => Unit (ignored).
      */
    def foreach(f: R => Unit): Unit = ()
  }

  /**
    * Unsuccessful match (Miss).
    *
    * @param t the value that was not matched.
    * @tparam T the underlying type of t.
    * @tparam R the result-type of this.
    */
  case class Miss[T, +R](msg: String, t: T) extends Unsuccessful[R, String](msg) {
    /**
      * @param f a function of R => S (ignored)
      * @tparam S the underlying type of the returned MatchResult.
      * @return Miss(t).
      */
    override def map[S](f: R => S): MatchResult[S] = Miss(msg, t)

    /**
      * Method to get the value of this MatchResult, otherwise return the parameter s.
      *
      * @param s a call-by-name value to be used if this is unsuccessful.
      * @tparam S a super-class of R.
      * @return s.
      */
    def getOrElse[S >: R](s: => S): S = s

    /**
      * Alternation method which takes a MatchResult as the alternative.
      *
      * @param sm a MatchResult which will be returned.
      * @tparam S the underlying type of the result and a super-class of R.
      * @return sm.
      */
    def orElse[S >: R](sm: => MatchResult[S]): MatchResult[S] = sm

    /**
      * Method to compose this MatchResult with sm.
      * It this is successful, then sm will be returned.
      * Otherwise, an Unsuccessful result will be returned.
      *
      * @param sm the MatchResult which must follow this MatchResult for a successful outcome.
      * @tparam S the type of the resulting MatchResult.
      * @return a Miss(msg, t).
      */
    def guard[S](sm: => MatchResult[S]): MatchResult[S] = Miss(msg, t)

    /**
      * @param sm a MatchResult[S] (ignored).
      * @tparam S the underlying type of s.
      * @return Miss(t).
      */
    def andThen[S](sm: => MatchResult[S]): MatchResult[R ~ S] = Miss(msg, t)

    /**
      * @param f a function of R => MatchResult[S] (ignored).
      * @tparam S the underlying type of the returned MatchResult.
      * @return Miss(t).
      */
    def flatMap[S](f: R => MatchResult[S]): MatchResult[S] = Miss(msg, t)

    /**
      * Alternation method which takes a Matcher as the alternative.
      * If this MatchResult is empty then return the value of m applied to the input.
      *
      * @param m a Matcher of Any to R.
      * @return m(t).
      */
    def |[S >: R](m: => Matcher[Any, S]): MatchResult[S] = m(t)

    /**
      * Composition method.
      * If this MatchResult is successful then return the value of m applied to the result.
      *
      * @param m a Matcher of S to T.
      * @tparam S the underlying type of the input to m (S is a super-class of R).
      * @tparam U the underlying type of the returned value.
      * @return a MatchResult[T].
      */
    def &[S >: R, U](m: => Matcher[S, U]): MatchResult[U] = Miss(msg, t)

    /**
      * @return a String representing this Miss.
      */
    override def toString: String = s"Miss: $msg: $t"
  }

  /**
    * Error when matching.
    *
    * @param e the exception that was thrown.
    * @tparam R the result-type of this.
    */
  case class Error[+R](e: Throwable) extends Unsuccessful[R, Throwable](e) {
    /**
      *
      * @param f a function of R => S (ignored)
      * @tparam S the underlying type of the returned MatchResult.
      * @return Error(e).
      */
    override def map[S](f: R => S): MatchResult[S] = Error(e)

    /**
      * Method to get the value of this MatchResult, otherwise return the parameter s.
      *
      * @param s a call-by-name value to be used if this is unsuccessful.
      * @tparam S a super-class of R.
      * @throws Throwable e
      */
    def getOrElse[S >: R](s: => S): S = get

    /**
      * Alternation method which takes a MatchResult as the alternative.
      *
      * @param sm a MatchResult which will be ignored.
      * @tparam S the underlying type of the result and a super-class of R.
      * @return sm.
      */
    def orElse[S >: R](sm: => MatchResult[S]): MatchResult[S] = Error(e)

    /**
      * Method to compose this MatchResult with sm.
      * It this is successful, then sm will be returned.
      * Otherwise, an Unsuccessful result will be returned.
      *
      * @param sm the MatchResult which must follow this MatchResult for a successful outcome.
      * @tparam S the type of the resulting MatchResult.
      * @return a MatchResult[S].
      */
    def guard[S](sm: => MatchResult[S]): MatchResult[S] = Error(e)

    /**
      * @param sm a MatchResult[S] (ignored).
      * @tparam S the underlying type of s.
      * @return Error(t).
      */
    def andThen[S](sm: => MatchResult[S]): MatchResult[R ~ S] = Error(e)

    /**
      * @param f a function of R => MatchResult[S].
      * @tparam S the underlying type of the returned MatchResult.
      * @return Error(e).
      */
    def flatMap[S](f: R => MatchResult[S]): MatchResult[S] = Error(e)

    /**
      * Alternation method which takes a Matcher as the alternative.
      * If this MatchResult is empty then return the value of m applied to the input.
      *
      * @param m a Matcher of Any to R.
      * @return m(t).
      */
    def |[S >: R](m: => Matcher[Any, S]): MatchResult[S] = Error(e)

    /**
      * Composition method.
      * If this MatchResult is successful then return the value of m applied to the result.
      *
      * @param m a Matcher of S to T.
      * @tparam S the underlying type of the input to m (S is a super-class of R).
      * @tparam U the underlying type of the returned value.
      * @return a MatchResult[T].
      */
    def &[S >: R, U](m: => Matcher[S, U]): MatchResult[U] = Error(e)

    override def toString: String = s"Error: ${e.getLocalizedMessage}"
  }

  object MatchResult {
    /**
      * Method to construct a MatchResult based on r.
      *
      * @param r a call-by-name value of R.
      * @tparam R the type of r and of the resulting MatchResult.
      * @return either a Match[R] or an Error[R].
      */
    def apply[R](r: => R): MatchResult[R] = Try(r) match {
      case Success(x) => Match(x)
      case Failure(e) => Error(e)
    }

    /**
      * Construct a MatchResult[R] based on a Boolean, a T and an R.
      * If b is true, then the result is Match(r); otherwise it is Miss(t).
      *
      * @param b a Boolean.
      * @param t a T.
      * @param r an R.
      * @tparam T input type.
      * @tparam R result type.
      * @return MatchResult[R].
      */
    def apply[T, R](b: Boolean, t: T, r: R): MatchResult[R] = if (b) Match(r) else Miss("false", t)

    /**
      * Construct a MatchResult[R] based on an Either[T, R].
      *
      * @param e either a Left[T] (miss) or a Right[R] (match).
      * @tparam T the Miss type.
      * @tparam R the Match type.
      * @return a MatchResult[R]
      */
    def apply[T, R](e: Either[T, R]): MatchResult[R] = e match {
      case Right(r) => Match(r)
      case Left(t) => Miss("left", t)
    }

    /**
      * Create a MatchResult based on an input value t, a result r, and a predicate p.
      *
      * @param p the predicate which determines whether the result is a Match or a Miss.
      * @param t the input value (ignored for a Match).
      * @param r the result value.
      * @tparam Q the control type.
      * @tparam T the input type.
      * @tparam R the result type.
      * @return a MatchResult[R].
      */
    def create[Q, T, R](p: (Q, R) => Boolean)(q: Q, t: T, r: R): MatchResult[R] = if (p(q, r)) Match(r) else Miss("create", t)

    /**
      * Yield a MatchResult[R] based on an input value t, a function f, and a predicate p.
      *
      * @param f the function T => R.
      * @param p the predicate which is applied to (q, r) to determines whether the result is a Match or a Miss,
      *          where r is the result of applying f to t.
      * @param q the control value.
      * @param t the input value.
      * @tparam Q the control type.
      * @tparam T the input type.
      * @tparam R the result type.
      * @return a MatchResult[R].
      */
    def apply[Q, T, R](f: T => R, p: (Q, R) => Boolean)(q: Q, t: T): MatchResult[R] = MatchResult.create(p)(q, t, f(t))
  }

  /**
    * Not sure why we need this but it's here.
    *
    * @param q a control value.
    * @param r a result value.
    * @tparam R the common type.
    * @return true if they are the same.
    */
  def isEqual[R](q: R, r: R): Boolean = q == r

  /**
    * (Should be) Private method to construct a Matcher.
    *
    * @param f a T => MatchResult[R].
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T, R] based on f.
    */
  private def constructMatcher[T, R](f: T => MatchResult[R]): Matcher[T, R] = (t: T) =>
    try f(t) catch {
      case e: MatchError => Miss(s"matchError: ${e.getLocalizedMessage}", t)
      case scala.util.control.NonFatal(e) => Error(e)
    }

  /**
    * Method to convert an Option of MatchResult[R] into a MatchResult of Option[R].
    *
    * @param rmo an optional MatchResult[R].
    * @tparam R the underlying result type.
    * @return a MatchResult of Option[R].
    */
  private def sequence[R](rmo: Option[MatchResult[R]]): MatchResult[Option[R]] = rmo match {
    case Some(rm) => rm.map(Some(_))
    case None => Match(None)
  }

  /**
    * Method to parse the String w according to the given regex and group indexes.
    *
    * @param regex a regular expression.
    * @param w     a String to be parsed (in its entirety).
    * @return a MatchResult[String].
    */
  private def doParse(regex: Regex, w: String): MatchResult[String] = {
    val matcher = regex.pattern.matcher(w)
    if (matcher.matches()) Match(matcher.group())
    else Miss(s"String $w did not match regex $regex", w)
  }

  /**
    * Method to parse the String w according to the given regex and group indexes.
    *
    * @param rg an instance of RegexGroups.
    * @param w  a String to be parsed (in its entirety).
    * @return a MatchResult of a List[String].
    */
  private def doParseGroups(rg: RegexGroups, w: String): MatchResult[List[String]] = rg.unapplySeq(w) match {
    case Some(ws) => Match(ws)
    case None => Miss(s"String $w did not match regex ${rg.regex}", w)
  }

  /**
    * Method to construct a Parser for the given regex such that a particular result type is returned.
    *
    * @param regex the regular expression to be matched.
    * @param name  the name of the parser method.
    * @param f     a function of String => Z.
    * @tparam Z the type to be returned from the resulting parser.
    * @return a Parser of type Z.
    */
  private def doParseWithParser[Z](regex: Regex, name: String)(f: String => MatchResult[Z]): Parser[Z] = Matcher {
    w =>
      doParse(regex, w) match {
        case Match(x) => Try(f(x)) match {
          case Success(m) => m
          case Failure(e) => Error(MatcherException(s"$name: $w caused function exception on: $x", e))
        }
        case Miss(z, x) => Miss(z, x)
        case Error(z) => Error(z)
        case _ => throw MatcherException("doParseWithParser: no match")
      }
  }

  /**
    * Method to construct a Parser for the given regex such that a particular result type is returned.
    *
    * @param rg   the regular expression to be matched.
    * @param name the name of the parser method.
    * @param f    a function of List[String] => Z.
    * @tparam Z the type to be returned from the resulting parser.
    * @return a Parser of type Z.
    */
  private def doParseGroupsWithFunction[Z](rg: RegexGroups, name: String)(f: List[String] => MatchResult[Z]): Parser[Z] = Matcher {
    w =>
      doParseGroups(rg, w) match {
        case Match(xs) => Try(f(xs)) match {
          case Success(m) => m
          case Failure(e) => Error(MatcherException(s"$name: $w matched incorrect number of groups: ${xs.size}", e))
        }
        case Miss(z, x) => Miss(z, x)
        case Error(z) => Error(z)
        case _ => throw MatcherException("doParseGroupsWithFunction: no match")
      }
  }

  lazy val wholeNumber: Parser[String] = parser("""-?\d+""")

  lazy val decimalNumber: Parser[String] = parser("""(\d+(\.\d*)?|\d*\.\d+)""")

  lazy val floatingPointNumber: Parser[String] = parser("""-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""")

}

/**
  * Companion object to Matchers.
  */
object Matchers {
  val matchers: Matchers = new Matchers {}

}

/**
  * A class combining a regular expression and a list of group indexes.
  *
  * @param regex  a Regex.
  * @param groups a list of required group indexes (if Nil, then all groups are selected).
  *               NOTE that the group indexes start at 1.
  */
case class RegexGroups(regex: Regex, groups: Seq[Int]) {
  def unapplySeq(s: CharSequence): Option[List[String]] = regex.unapplySeq(s) map selectGroups

  private def selectGroups(ws: List[String]): List[String] = groups match {
    case Nil => ws
    case groups => (for (group <- groups) yield ws(group - 1)).toList
  }

}

object RegexGroups {
  def apply(regex: Regex): RegexGroups = apply(regex, Nil)

  def apply(regex: String): RegexGroups = apply(new Regex(regex))

  def create(regex: Regex, groups: Int*): RegexGroups = apply(regex, groups)

  def create(regex: String, groups: Int*): RegexGroups = apply(new Regex(regex), groups)
}

/**
  * The tilde class which is basically a Tuple (i.e. Product) with a few extra methods.
  *
  * @param l the left value.
  * @param r the right value.
  * @tparam L the left type.
  * @tparam R the right type.
  */
case class ~[+L, +R](l: L, r: R) {
  /**
    * Reverse the order of this ~.
    *
    * @return ~(r, l)
    */
  def flip: ~[R, L] = new ~(r, l)

  /**
    * Convert to tuple form.
    *
    * NOTE: unused.
    *
    * @return a (L, R).
    */
  def asTuple: (L, R) = l -> r

  override def toString: String = s"$l~$r"
}

/**
  * Companion object to ~ (although the name had to be changed).
  */
object Tilde {
  /**
    * Create a ~ from two values.
    *
    * @param l the left-hand value.
    * @param r the right-hand value.
    * @tparam L the type of l.
    * @tparam R the type of r.
    * @return a new L ~ R
    */
  def apply[L, R](l: L, r: R): ~[L, R] = new ~(l, r)

  /**
    * Create a ~ from an (L, R) tuple.
    *
    * @param t a tuple (L, R).
    * @tparam L the type of the first member of the input.
    * @tparam R the type of r the second member of the input.
    * @return a new L ~ R
    */
  def apply[L, R](t: (L, R)): ~[L, R] = apply(t._1, t._2)
}

/**
  * A MatcherException.
  *
  * @param msg a message.
  * @param x   a Throwable.
  */
case class MatcherException(msg: String, x: Throwable) extends Exception(msg, x)

object MatcherException {
  def apply(msg: String): MatcherException = MatcherException(msg, null)
}

import org.slf4j.Logger

/**
  * Trait which is used to define a logging level for the log method of SignificantSpaceParsers.
  */
trait LogLevel

case object LogDebug extends LogLevel

case object LogInfo extends LogLevel

case object LogOff extends LogLevel

object LogLevel {
  implicit val ll: LogLevel = LogOff
}

class MatchLogger(val logLevel: LogLevel, f: String => Unit) extends ((String => Unit)) {
  override def apply(w: String): Unit =
    logLevel match {
      case LogInfo | LogDebug => f(w)
      case _ =>
    }
}

case class Slf4jLogger(override val logLevel: LogLevel, logger: Logger) extends MatchLogger(logLevel, w => {
  logLevel match {
    case LogInfo => logger.info(w)
    case LogDebug => logger.debug(w)
    case _ =>
  }
})

object MatchLogger {

  import org.slf4j.LoggerFactory

  def apply(logLevel: LogLevel, clazz: Class[_]): MatchLogger = Slf4jLogger(logLevel, LoggerFactory.getLogger(clazz))

  def apply(logger: Logger): MatchLogger = Slf4jLogger(LogInfo, logger)
}
