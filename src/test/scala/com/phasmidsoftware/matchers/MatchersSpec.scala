package com.phasmidsoftware.matchers

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.NoSuchElementException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Success, Try}

class MatchersSpec extends AnyFlatSpec with should.Matchers {

  import Matchers._

  private val m = matchers

  behavior of "MatchResult"
  it should "implement apply(Boolean, T, R)" in {
    m.MatchResult(b = true, "1", 1) shouldBe m.Match(1)
    m.MatchResult(b = false, "1", 1) shouldBe m.Miss("false", "1")
  }
  it should "implement apply(Either)" in {
    val e1: Either[String, Int] = Left("1")
    val e2: Either[String, Int] = Right(1)
    m.MatchResult(e2) shouldBe m.Match(1)
    m.MatchResult(e1) shouldBe m.Miss("left", "1")
  }
  it should "implement apply((Q,R) => Boolean)" in {
    val v1: m.MatchResult[Int] = m.MatchResult.create[Int, String, Int](m.isEqual)(1, "1", 1)
    v1 shouldBe m.Match(1)
    m.MatchResult.create[Int, String, Int](m.isEqual)(1, "1", 2) shouldBe m.Miss("create", "1")
  }
  it should "implement apply(T=>R, (Q,R) => Boolean))" in {
    val v1: m.MatchResult[Int] = m.MatchResult[Int, String, Int]({ s: String => s.toInt }, m.isEqual)(1, "1")
    v1 shouldBe m.Match(1)
    m.MatchResult.create[Int, String, Int](m.isEqual)(1, "1", 2) shouldBe m.Miss("create", "1")
  }

  behavior of "Match"

  it should "support good success" in {
    val target = new m.Match[Int](0)
    val value1 = target.success(1)
    value1.successful shouldBe true
    value1.getOrElse(0) shouldBe 1
  }
  it should "support bad success" in {
    val target = new m.Match[Int](0)
    val value1 = target.success(1 / 0)
    value1.successful shouldBe false
  }
  it should "support successful" in {
    m.success(0)("").successful shouldBe true
  }
  it should "support isEmpty" in {
    m.success(0)("").isEmpty shouldBe false
  }
  it should "support always" in {
    val target = m.always[Unit]
    target(()).successful shouldBe true
  }
  // NOTE that ~ is a synonym of andThen.
  it should "support Match ~" in {
    m.Match(0) ~ m.Match("") should matchPattern { case m.Match(~(0, "")) => }
    m.Match(0) ~ m.Miss("bad", "") shouldBe m.Miss("bad", "")
    m.Match(0) ~ m.Error(new RuntimeException("")) should matchPattern { case m.Error(_: RuntimeException) => }
  }
  it should "support Miss ~" in {
    m.Miss("bad", 0) ~ m.Match("") shouldBe m.Miss("bad", 0)
    m.Miss("bad", 0) ~ m.Miss("bad", "") shouldBe m.Miss("bad", 0)
    m.Miss("bad", 0) ~ m.Error(new RuntimeException("")) shouldBe m.Miss("bad", 0)
  }
  it should "support Error ~" in {
    m.Error(new RuntimeException("")) ~ m.Match("") should matchPattern { case m.Error(_: RuntimeException) => }
    m.Error(new RuntimeException("")) ~ m.Miss("bad", "") should matchPattern { case m.Error(_: RuntimeException) => }
    m.Error(new RuntimeException("")) ~ m.Error(new RuntimeException("")) should matchPattern { case m.Error(_: RuntimeException) => }
  }
  // NOTE that && is a synonym of guard.
  it should "support Match &&" in {
    m.Match(0) && m.Match("") shouldBe m.Match("")
    m.Match(0) && m.Miss("bad", "") shouldBe m.Miss("bad", "")
    m.Match(0) && m.Error(new RuntimeException("")) should matchPattern { case m.Error(_: RuntimeException) => }
  }
  it should "support Miss &&" in {
    m.Miss("bad", 0) && m.Match("") shouldBe m.Miss("bad", 0)
    m.Miss("bad", 0) && m.Miss("bad", "") shouldBe m.Miss("bad", 0)
    m.Miss("bad", 0) && m.Error(new RuntimeException("")) shouldBe m.Miss("bad", 0)
  }
  it should "support Error &&" in {
    m.Error(new RuntimeException("")) && m.Match("") should matchPattern { case m.Error(_: RuntimeException) => }
    m.Error(new RuntimeException("")) && m.Miss("bad", "") should matchPattern { case m.Error(_: RuntimeException) => }
    m.Error(new RuntimeException("")) && m.Error(new RuntimeException("")) should matchPattern { case m.Error(_: RuntimeException) => }
  }
  it should "support |" in {
    val result = m.success(0)("") | m.fail("")
    result.successful shouldBe true
  }
  it should "support ||" in {
    val result = m.success(0)("") || m.fail("")("")
    result.successful shouldBe true
  }
  it should "support &" in {
    val result = m.success(0)("") & m.success[Any, Int](0)
    result.successful shouldBe true
  }
  it should "support &&" in {
    val result = m.success(0)("") ~ m.success(1)("")
    result.successful shouldBe true
    import m.TildeOps
    result.get shouldBe 0 ~ 1
  }
  it should "support map" in {
    val result = m.success(0)("").map(_.toString)
    result.successful shouldBe true
    result.getOrElse("") shouldBe "0"
  }
  it should "support flatMap" in {
    val result = m.success(0)("").flatMap(x => m.Match(x.toString))
    result.successful shouldBe true
    result.getOrElse("") shouldBe "0"
  }
  it should "support getOrElse" in {
    val result = m.success(0)("").flatMap(x => m.Match(x.toString))
    result.getOrElse("qq") shouldBe "0"
  }

  behavior of "Miss"

  it should "support toString" in {
    val target = m.fail("error")
    target(1).toString shouldBe "Miss: error: 1"
  }
  it should "support successful" in {
    m.fail("error")("").successful shouldBe false
  }
  it should "support isEmpty" in {
    m.fail("")("").isEmpty shouldBe true
  }
  it should "support error" in {
    val target = m.error[Unit](new NoSuchElementException)
    target(()).successful shouldBe false
  }
  it should "support |" in {
    val result = m.fail("")("") | m.success(0)
    result.successful shouldBe true
  }
  it should "support ||" in {
    val result = m.fail("")("") || m.success(0)("")
    result.successful shouldBe true
    result.get shouldBe 0
  }
  it should "support &" in {
    val result = m.fail("")("") & m.success[Any, Int](0)
    result.successful shouldBe false
  }
  it should "support &&" in {
    val result = m.fail("")("") ~ m.success(1)("")
    result.successful shouldBe false
  }
  it should "support map" in {
    val result = m.fail[String, Int]("")("").map(_.toString)
    result.successful shouldBe false
    a[MatcherException] shouldBe thrownBy(result.get)
  }
  it should "support flatMap" in {
    val result = m.fail[String, Int]("")("").flatMap(x => m.Match(x.toString))
    result.successful shouldBe false
    a[MatcherException] shouldBe thrownBy(result.get)
  }
  it should "support foreach" in {
    val result = m.fail[String, Int]("")("")
    val sb = new StringBuilder
    result.foreach { x => sb.append(x); () }
    sb.toString().length shouldBe 0
  }
  it should "support getOrElse" in {
    val result = m.fail[String, Int]("")("")
    result.getOrElse("qq") shouldBe "qq"
  }

  behavior of "Error"

  it should "support toString" in {
    val target = m.error[Unit](new NoSuchElementException)
    target(1).toString shouldBe "Error: null"
  }
  it should "support successful" in {
    m.error[Unit](new NoSuchElementException)(()).successful shouldBe false
  }
  it should "support isEmpty" in {
    m.error[Unit](new NoSuchElementException)(()).isEmpty shouldBe true
  }
  it should "support |" in {
    val result = m.error[Unit](new NoSuchElementException)(()) | m.success(0)
    result.successful shouldBe false
  }
  it should "support ||" in {
    val result = m.error[Unit](new NoSuchElementException)(()) || m.success(0)(0)
    result.successful shouldBe false
  }
  it should "support &" in {
    val result = m.error[Unit](new NoSuchElementException)(()) & m.success[Any, Int](0)
    result.successful shouldBe false
  }
  it should "~" in {
    val result = m.error[Unit](new NoSuchElementException)(()) ~ m.success(1)("")
    result.successful shouldBe false
  }
  it should "support map" in {
    val result = m.error[Unit](new NoSuchElementException)(()).map(_.toString)
    result.successful shouldBe false
    a[NoSuchElementException] shouldBe thrownBy(result.getOrElse(""))
  }
  it should "support flatMap" in {
    val result = m.error[Unit](new NoSuchElementException)(()).flatMap(x => m.Match(x.toString))
    result.successful shouldBe false
    a[NoSuchElementException] shouldBe thrownBy(result.getOrElse(""))
  }
  it should "support foreach" in {
    val result = m.error[Unit](new NoSuchElementException)(())
    val sb = new StringBuilder
    result.foreach { x => sb.append(x); () }
    sb.toString().length shouldBe 0
  }
  it should "support getOrElse" in {
    val result = m.error[Unit](new NoSuchElementException)(())
    a[NoSuchElementException] shouldBe thrownBy(result.getOrElse(1))
  }

  behavior of "Matcher class"

  it should "support map" in {
    val target = new m.Matcher[String, Int] {
      def apply(v1: String): m.MatchResult[Int] = m.Match(v1.toInt)
    }
    val q: m.Parser[Double] = target map (_.toDouble)
    q("1") shouldBe m.Match(1.0)
  }
  it should "support flatMap" in {
    val target = new m.Matcher[String, Int] {
      def apply(v1: String): m.MatchResult[Int] = m.Match(v1.toInt)
    }
    val q: m.Matcher[String, Double] = target flatMap { x => m.Match(x.toDouble) }
    q("1") shouldBe m.Match(1.0)
  }
  it should "support unit" in {
    val target = new m.Matcher[String, Int] {
      def apply(v1: String): m.MatchResult[Int] = m.Match(v1.toInt)
    }
    val q: m.Matcher[String, Int] = target.unit[Int](2)
    q("1") shouldBe m.Match(2)
  }
  it should "support ^^" in {
    val target = new m.Matcher[String, Int] {
      def apply(v1: String): m.MatchResult[Int] = m.Match(v1.toInt)
    }
    val q: m.Matcher[String, Double] = target ^^ (_.toDouble)
    q("1") shouldBe m.Match(1.0)
  }
  it should "support !" in {
    val target = new m.Matcher[String, Int] {
      def apply(v1: String): m.MatchResult[Int] = m.Match(v1.toInt)
    }
    val q: m.Matcher[String, Int] = target.![Int](1)
    val result = q("1")
    result.successful shouldBe false
    result shouldBe m.Miss("not", "1")
  }

  behavior of "Matcher method"

  it should "work with fixed success result" in {
    val f: m.Parser[Int] = m.Matcher(_ => m.Match(1))
    f("1").successful shouldBe true
  }
  it should "work with fixed fail result" in {
    val f: m.Parser[Int] = m.Matcher(e => m.Miss("", e))
    f("1").successful shouldBe false
  }
  it should "result in error when function throws exception" in {
    def f(x: String): m.MatchResult[Int] = m.Match(x.toInt)

    val p: m.Parser[Int] = m.Matcher(f)
    val result = p("x")
    result.successful shouldBe false
    result should matchPattern { case m.Error(_) => }
    a[NumberFormatException] shouldBe thrownBy(result.getOrElse(""))
  }

  behavior of "log"
  it should "log success with LogDebug" in {
    val sb = new StringBuilder
    implicit val logger: MatchLogger = new MatchLogger(LogDebug, { w => sb.append(s"$w\n"); () }, 0)
    import m.MatcherOps
    val p = m.success(1) :| "success(1)"
    p(1).successful shouldBe true
    sb.toString() shouldBe
            """  trying matcher success(1) on 1...
              |  ... success(1): Match: 1
              |""".stripMargin
  }
  it should "log success with LogInfo" in {
    import m.MatcherOps
    val sb = new StringBuilder
    implicit val logger: MatchLogger = SBLogger(LogInfo, sb)
    val p = m.success(1) :| "success(1)"
    p(1).successful shouldBe true
    sb.toString() shouldBe
            """  success(1): matched 1
              |""".stripMargin
  }
  it should "log success with LogOff" in {
    val sb = new StringBuilder
    import m.MatcherOps
    implicit val logger: MatchLogger = SBLogger(LogOff, sb)
    val p = m.success(1) :| "success(1)"
    p(1).successful shouldBe true
    sb.toString() shouldBe ""
  }
  it should "handle nested matchers" in {
    val sb = new StringBuilder
    implicit val logger: MatchLogger = SBLogger(LogDebug, sb)
    import m.MatcherOps
    val m1 = m.matches(1) :| "one"
    val m2 = m.matches(2) :| "two"
    val x = (m1 | m2) :| "one or two"
    val z = x(3)
    println(z)
    z should matchPattern { case m.Miss(_, _) => }
    println(sb)
    sb.toString() shouldBe "  trying matcher one or two on 3...\n  trying matcher one on 3...\n  ... one: Miss: filter: 3\n  trying matcher two on 3...\n  ... two: Miss: filter: 3\n  ... one or two: Miss: filter: 3\n"
  }

  // CONSIDER eliminating the namedMatcher method.
  behavior of "namedMatcher"
  it should "work with fixed success result" in {
    val sb = new StringBuilder
    implicit val logger: MatchLogger = SBLogger(LogDebug, sb)
    val f: m.Parser[Int] = m.namedMatcher("one")(_ => m.Match(1))
    f("1").successful shouldBe true
    sb.toString() shouldBe
            """  trying matcher one on 1...
              |  ... one: Match: 1
              |""".stripMargin
  }

  behavior of "tildeOps"
  it should "support ~" in {
    import m.TildeOps
    val x = 1 ~ 2
    x shouldBe new ~(1, 2)
  }
  behavior of "success"
  it should "work with 1" in {
    val f = m.success[String, Int](1)
    f("1").successful shouldBe true
  }

  behavior of "fail"
  it should "work with fixed fail result" in {
    val f = m.fail[String, Int]("")
    f("").successful shouldBe false
  }

  behavior of "maybe"
  it should "work with fixed fail result" in {
    val fTrue = m.maybe[String](b = true)
    fTrue("").successful shouldBe true
    val fFalse = m.maybe[String](b = false)
    fFalse("").successful shouldBe false
  }

  behavior of "matches"
  it should "match 1" in {
    val f = m.matches(1)
    f(1).successful shouldBe true
  }

  behavior of "alt"
  it should "match 1" in {
    val f = m.alt(m.matches(1))
    f(1).successful shouldBe true
  }
  it should "match 2" in {
    val f = m.alt(m.matches(2))
    f(1).successful shouldBe true
  }

  behavior of "valve (1)"
  it should "match 1" in {
    val p: (Int, Int) => Boolean = {
      case (x, y) => x == y
    }
    val f: m.Matcher[(Int, Int), Int] = m.valve(p)
    f(1, 1).successful shouldBe true
  }
  it should "match parity" in {
    val p: (Int, Int) => Boolean = {
      case (x, y) => y % 2 == x
    }
    val f: m.Matcher[(Int, Int), Int] = m.valve(p)
    val odd = 1
    val even = 0
    f(odd, 3) shouldBe m.Match(3)
    f(even, 4) shouldBe m.Match(4)
    f(even, 3) shouldBe m.Miss("create", 3)
  }

  behavior of "valve (2)"
  it should "match 1" in {
    val p: (Int, Int) => Boolean = {
      case (x, y) => x == y
    }
    val sb = new StringBuilder
    implicit val logger: MatchLogger = SBLogger(LogDebug, sb)
    val z: m.Matcher[(Int, String), Int] = m.valve(_.toInt, p)
    z(1, "1").successful shouldBe true
  }
  it should "match parity" in {
    val p: (Int, Int) => Boolean = {
      case (x, y) => y % 2 == x
    }
    val sb = new StringBuilder
    implicit val logger: MatchLogger = SBLogger(LogDebug, sb)
    val z: m.Matcher[(Int, String), Int] = m.valve(_.toInt, p)
    val odd = 1
    val even = 0
    z(odd, "3") shouldBe m.Match(3)
    z(even, "4") shouldBe m.Match(4)
    z(even, "3") shouldBe m.Miss("create", "3")
  }

  behavior of "chain"
  it should "work for first form" in {
    val target = m.lift[String, Int](_.toInt)
    val p = m.valve[Int, Int] { case (q, r) => q == r }
    val z = target chain p
    z(1, "1").successful shouldBe true
  }
  it should "work for second form" in {
    val target = m.lift[String, Int](_.toInt)
    val p = m.matches[(String, Int)](("1", 1))
    val z = target chain p
    z("1", "1") shouldBe m.Match(("1", 1))
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    val f = m.matches(1)
    val g = f | m.matches(2)
    g(1).successful shouldBe true
    g(2).successful shouldBe true
  }

  behavior of "not"
  it should "work for match" in {
    val p = m.matches(1)
    val q = m.not(p, 0)
    p(1).successful shouldBe true
    q(1).successful shouldBe false
    q(2).successful shouldBe true
    q(2).getOrElse(-1) shouldBe 0
  }
  it should "work for error situation" in {
    val p = m.Matcher[Int, Int](_ => m.Error(new NoSuchElementException))
    val q = m.not(p, 0)
    p(1).successful shouldBe false
    q(1).successful shouldBe false
    q(2).successful shouldBe false
  }

  behavior of "swap"
  it should "work" in {
    import m.TildeOps
    val t = 1 ~ "1"
    val u: m.MatchResult[String ~ Int] = m.swap(t)
    u shouldBe m.Match("1" ~ 1)
  }

  behavior of "*"
  it should "work with default parameter" in {
    import m.TildeOps
    val t = 1 ~ 2
    val p: m.Matcher[Int ~ Int, Int ~ Int] = m.filter2_0(m.matches(2))
    p(t).successful shouldBe false
    m.*(p)(t).successful shouldBe true
  }
  it should "not work with false" in {
    import m.TildeOps
    val t = 1 ~ 2
    val p: m.Matcher[Int ~ Int, Int ~ Int] = m.filter2_0(m.matches(2))
    p(t).successful shouldBe false
    m.*(p, commutes = false)(t).successful shouldBe false
  }

  behavior of "**"
  it should "match 1 with commuting" in {
    import m.TildeOps
    val t = 1 ~ 2 ~ 3
    val p: m.Matcher[Int ~ Int ~ Int, Int ~ Int ~ Int] = m.filter3_0(m.matches(1))
    p(t).successful shouldBe true
    m.**(p)(t).successful shouldBe true
  }
  it should "match 2 with commuting" in {
    import m.TildeOps
    val t = 1 ~ 2 ~ 3
    val p: m.Matcher[Int ~ Int ~ Int, Int ~ Int ~ Int] = m.filter3_0(m.matches(2))
    p(t).successful shouldBe false
    m.**(p)(t).successful shouldBe true
  }
  it should "match 3 with commuting" in {
    import m.TildeOps
    val t = 1 ~ 2 ~ 3
    val p: m.Matcher[Int ~ Int ~ Int, Int ~ Int ~ Int] = m.filter3_0(m.matches(3))
    p(t).successful shouldBe false
    m.**(p)(t).successful shouldBe true
  }
  it should "fail without commuting" in {
    import m.TildeOps
    val t = 1 ~ 2 ~ 3
    val p: m.Matcher[Int ~ Int ~ Int, Int ~ Int ~ Int] = m.filter3_0(m.matches(2))
    m.**(p, commutes = false)(t).successful shouldBe false
  }

  behavior of "filter"
  it should "filter2_0" in {
    import m.TildeOps
    val t = 1 ~ 2
    val p1: m.Matcher[Int ~ Int, Int ~ Int] = m.filter2_0(m.matches(2))
    p1(t).successful shouldBe false
    val p2: m.Matcher[Int ~ Int, Int ~ Int] = m.filter2_0(m.matches(1))
    p2(t).successful shouldBe true
  }
  it should "filter2_1" in {
    import m.TildeOps
    val t = "1" ~ 2
    val p1: m.Matcher[String ~ Int, String ~ Int] = m.filter2_1(m.matches(2))
    p1(t).successful shouldBe true
    val p2: m.Matcher[String ~ Int, String ~ Int] = m.filter2_1(m.matches(1))
    p2(t).successful shouldBe false
  }
  it should "filter3_0" in {
    import m.TildeOps
    val t = "1" ~ 2 ~ 3.0
    val p1: m.Matcher[String ~ Int ~ Double, String ~ Int ~ Double] = m.filter3_0(m.matches("1"))
    p1(t).successful shouldBe true
    val p2: m.Matcher[String ~ Int ~ Double, String ~ Int ~ Double] = m.filter3_0(m.matches("2"))
    p2(t).successful shouldBe false
  }
  it should "filter3_1" in {
    import m.TildeOps
    val t = "1" ~ 2 ~ 3.0
    val p1: m.Matcher[String ~ Int ~ Double, String ~ Int ~ Double] = m.filter3_1(m.matches(2))
    p1(t).successful shouldBe true
    val p2: m.Matcher[String ~ Int ~ Double, String ~ Int ~ Double] = m.filter3_1(m.matches(1))
    p2(t).successful shouldBe false
  }
  it should "filter3_2" in {
    import m.TildeOps
    val t = "1" ~ 2 ~ 3.0
    val p1: m.Matcher[String ~ Int ~ Double, String ~ Int ~ Double] = m.filter3_2(m.matches(3.0))
    p1(t).successful shouldBe true
    val p2: m.Matcher[String ~ Int ~ Double, String ~ Int ~ Double] = m.filter3_2(m.matches(0))
    p2(t).successful shouldBe false
  }

  behavior of "rotate3"
  it should "work" in {
    import m.TildeOps
    val t = 1 ~ "1" ~ 1.0
    val u: m.MatchResult[String ~ Double ~ Int] = m.rotate3(t)
    u shouldBe m.Match("1" ~ 1.0 ~ 1)
  }

  behavior of "invert3"
  it should "work" in {
    import m.TildeOps
    val t = 1 ~ "1" ~ 1.0
    val u: m.MatchResult[Double ~ String ~ Int] = m.invert3(t)
    u shouldBe m.Match(1.0 ~ "1" ~ 1)
  }

  behavior of "~"
  it should "match (1,2) and result in (1,2)" in {
    import m.TildeOps
    val p = m.matches(1)
    val q = m.matches(2)
    val z = p ~ q
    val result = z(1 ~ 2)
    result.successful shouldBe true
    result.get shouldBe Tilde(1, 2)
  }
  it should "match (1,2) and result in 2" in {
    import m.TildeOps
    val p = m.matches(1)
    val q = m.matches(2)
    val z = p ~> q
    val result = z(1 ~ 2)
    result.successful shouldBe true
    result.getOrElse(0) shouldBe 2
  }
  it should "match (1,2) and result in 1" in {
    import m.TildeOps
    val p = m.matches(1)
    val q = m.matches(2)
    val z = p <~ q
    val result = z(1 ~ 2)
    result.successful shouldBe true
    result.getOrElse(0) shouldBe 1
  }

  behavior of "select"
  it should "select2_0" in {
    case class Complex(r: Double, i: Double)
    val z: m.Matcher[Complex, Double] = m.select2_0(Complex)
    val result = z(Complex(1, 0))
    result.successful shouldBe true
    result.getOrElse(0) shouldBe 1
  }
  it should "select2_1" in {
    case class Complex(r: Double, i: Double)
    val z: m.Matcher[Complex, Double] = m.select2_1(Complex)
    val result = z(Complex(1, 0))
    result.successful shouldBe true
    result.getOrElse(Double.NaN) shouldBe 0
  }
  it should "select3_0" in {
    case class Vector(x: String, y: Int, z: Double)
    val z: m.Matcher[Vector, Double] = m.select3_0(Vector)
    val result = z(Vector("1", 0, 0.5))
    result.successful shouldBe true
    result.getOrElse("") shouldBe "1"
  }
  it should "select3_1" in {
    case class Vector(x: String, y: Int, z: Double)
    val z: m.Matcher[Vector, Int] = m.select3_1(Vector)
    val result = z(Vector("1", 0, 0.5))
    result.successful shouldBe true
    result.getOrElse(-1) shouldBe 0
  }
  it should "select3_2" in {
    case class Vector(x: String, y: Int, z: Double)
    val z: m.Matcher[Vector, Double] = m.select3_2(Vector)
    val result = z(Vector("1", 0, 0.5))
    result.successful shouldBe true
    result.getOrElse(Double.NaN) shouldBe 0.5
  }

  behavior of "having"

  case class Wrapper(i: Int)

  it should "work" in {
    val p = m.matches(1)
    val q: m.Matcher[Wrapper, Int] = m.having(p)(_.i)
    q(Wrapper(1)).successful shouldBe true
  }

  behavior of "matchProduct2Any"

  case class StringPair(t1: String, t2: String)

  it should "succeed with toInt and 0" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.success(0)
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.fail("")
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.fail("")
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(q, p)(StringPair)
    val tuple = StringPair("", "1")
    r(tuple).successful shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Parser[Int] = m.fail("")
    val q: m.Parser[Int] = m.fail("")
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe false
  }

  behavior of "matchProduct2All"
  it should "succeed with toInt and 0" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.success(0)
    val r: m.Matcher[StringPair, Int ~ Int] = m.matchProduct2All(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }

  behavior of "matchProduct3All"

  case class Triple(t1: String, t2: String, t3: Int)

  it should "succeed with toInt and 0 and identity" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.success(0)
    val z: m.Matcher[Int, Int] = m.lift(identity)
    val r: m.Matcher[Triple, Int ~ Int ~ Int] = m.matchProduct3All(p, q, z)(Triple.apply)
    val tuple = Triple("1", "", 0)
    r(tuple).successful shouldBe true
  }

  behavior of "flip"
  it should "work" in {
    import m.TildeOps
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Matcher[Int, Double] = m.lift(_.toDouble)
    val z: m.Matcher[String ~ Int, Int ~ Double] = p ~ q
    z("1" ~ 1) should matchPattern { case m.Match(~(1, 1.0)) => }
    m.flip(z)(1 ~ "1") should matchPattern { case m.Match(~(1, 1.0)) => }
  }

  behavior of "tilde2"
  it should "work" in {
    val p: m.Matcher[StringPair, String ~ String] = m.tilde2(StringPair)
    p(StringPair("x", "y")) should matchPattern { case m.Match("x" ~ "y") => }
  }

  behavior of "tilde3"
  it should "work" in {
    val p: m.Matcher[Triple, String ~ String ~ Int] = m.tilde3(Triple)
    p(Triple("x", "y", 1)) should matchPattern { case m.Match("x" ~ "y" ~ 1) => }
  }

  behavior of "product2"
  it should "one-way" in {
    val p: m.Matcher[String ~ String, StringPair] = m.product2(StringPair)
    import m.TildeOps
    p("x" ~ "y") should matchPattern { case m.Match(StringPair("x", "y")) => }
  }
  it should "round-trip" in {
    val p: m.Matcher[StringPair, StringPair] = m.tilde2(StringPair) & m.product2(StringPair)
    p(StringPair("x", "y")) should matchPattern { case m.Match(StringPair("x", "y")) => }
  }

  behavior of "product3"
  it should "one-way" in {
    val p: m.Matcher[String ~ String ~ Int, Triple] = m.product3(Triple)
    import m.TildeOps
    p("x" ~ "y" ~ 1) should matchPattern { case m.Match(Triple("x", "y", 1)) => }
  }
  it should "round-trip" in {
    val p: m.Matcher[Triple, Triple] = m.tilde3(Triple) & m.product3(Triple)
    p(Triple("x", "y", 1)) should matchPattern { case m.Match(Triple("x", "y", 1)) => }
  }

  behavior of "match2Any"
  it should "succeed with toInt and 0" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.success(0)
    val r: m.Matcher[String ~ String, Int] = m.match2Any(p, q)
    import m.TildeOps
    val tilde = "1" ~ ""
    r(tilde).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.fail("")
    val r: m.Matcher[String ~ String, Int] = m.match2Any(p, q)
    import m.TildeOps
    val tilde = "1" ~ ""
    r(tilde).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.fail("")
    val r: m.Matcher[String ~ String, Int] = m.match2Any(q, p)
    import m.TildeOps
    val tilde = "" ~ "1"
    r(tilde).successful shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Parser[Int] = m.fail("")
    val q: m.Parser[Int] = m.fail("")
    val r: m.Matcher[String ~ String, Int] = m.match2Any(q, p)
    import m.TildeOps
    val tilde = "1" ~ ""
    r(tilde).successful shouldBe false
  }

  behavior of "match3Any"
  it should "succeed with toInt and 0" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.success(0)
    val z: m.Parser[Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.fail("")
    val z: m.Parser[Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.fail("")
    val z: m.Parser[Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Parser[Int] = m.fail("")
    val q: m.Parser[Int] = m.fail("")
    val z: m.Parser[Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    val result = r(tuple)
    result should matchPattern { case m.Error(_: ArithmeticException) => }
  }

  behavior of "match2All"
  it should "succeed with toInt and 0" in {
    import m.TildeOps
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.success(0)
    val r: m.Matcher[String ~ String, Int ~ Int] = m.match2All(p, q)
    val tuple = "1" ~ ""
    r(tuple).successful shouldBe true
  }
  it should "fail with toInt and fail" in {
    import m.TildeOps
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.fail("")
    val r: m.Matcher[String ~ String, Int ~ Int] = m.match2All(p, q)
    val tuple = "1" ~ ""
    r(tuple).successful shouldBe false
  }
  it should "fail with fail and toInt" in {
    import m.TildeOps
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.fail("")
    val r: m.Matcher[String ~ String, Int ~ Int] = m.match2All(q, p)
    val tuple = "" ~ "1"
    r(tuple).successful shouldBe false
  }
  it should "fail with fail and fail" in {
    import m.TildeOps
    val p: m.Parser[Int] = m.fail("")
    val q: m.Parser[Int] = m.fail("")
    val r: m.Matcher[String ~ String, Int ~ Int] = m.match2All(q, p)
    val tuple = "" ~ "1"
    r(tuple).successful shouldBe false
  }

  behavior of "match3All"
  it should "succeed with toInt and 0" in {
    import m.TildeOps
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.success(0)
    val z: m.Parser[Int] = m.success(1)
    val r: m.Matcher[String ~ String ~ String, Int ~ Int ~ Int] = m.match3All(p, q, z)
    val tuple = "1" ~ "" ~ "junk"
    r(tuple).successful shouldBe true
  }
  it should "fail with toInt and fail" in {
    import m.TildeOps
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Int] = m.fail("")
    val z: m.Parser[Int] = m.success(1 / 0)
    val r: m.Matcher[String ~ String ~ String, Int ~ Int ~ Int] = m.match3All(p, q, z)
    val tuple = "1" ~ "" ~ "junk"
    r(tuple).successful shouldBe false
  }

  behavior of "opt"
  it should "succeed with a match" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q = m.opt(p)
    q("1") should matchPattern { case m.Match(Some(1)) => }
  }
  it should "match None with null" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q = m.opt(p)
    q(null) should matchPattern { case m.Match(None) => }
  }
  it should "error with a bad conversion" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q = m.opt(p)
    val mr = q("a")
    mr should matchPattern { case m.Error(_) => }
  }
  it should "miss with a bad match" in {
    val p: m.Parser[String] = m.matches("")
    val q = m.opt(p)
    val mr = q("a")
    mr should matchPattern { case m.Miss(_, _) => }
  }

  behavior of "?"
  it should "succeed with a match" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Option[Int]] = p.?
    q("1") should matchPattern { case m.Match(Some(1)) => }
  }

  behavior of "trial"
  it should "succeed with a match" in {
    val p: m.Parser[Int] = m.lift(_.toInt)
    val q: m.Parser[Try[Int]] = p.trial
    q("1") should matchPattern { case m.Match(Success(1)) => }
  }

  behavior of "tee"
  it should "work" in {
    val r: m.MatchResult[Int] = m.Match(1)
    val s = new StringBuilder
    import m.MatchResultOps
    (r :- { x => s.append(x.toString); () }).successful shouldBe true
    s.toString() shouldBe "1"
  }

  behavior of "parsers"
  it should "parse 12345 as Int" in {
    val p = m.parserInt
    p("12345") shouldBe m.Match(12345)
  }
  it should "parse 12345 as Long" in {
    val p = m.parserLong
    p("12345") shouldBe m.Match(12345L)
  }
  it should "parse 12345 as BigInt" in {
    val p = m.parserBigInt
    p("12345") shouldBe m.Match(BigInt(12345))
  }
  it should "parse 3.1415927 as Double" in {
    val p = m.parserDouble
    p("3.1415927") shouldBe m.Match(3.1415927)
  }
  it should "parse 3.1415927 as BigDecimal" in {
    val p = m.parserBigDecimal
    p("3.1415927") shouldBe m.Match(BigDecimal("3.1415927"))
  }
  it should "parseWithParser given String" in {
    val p: m.Parser[LocalDate] = m.parseWithParser("""\d+-\d+-\d+""")(m.lift(s => LocalDate.parse(s, DateTimeFormatter.ISO_DATE)))
    p("2021-06-07") shouldBe m.Match(LocalDate.parse("2021-06-07"))
  }
  it should "parseWithParser given regex" in {
    val p: m.Parser[LocalDate] = m.parseWithParser("""\d+-\d+-\d+""".r)(m.lift(s => LocalDate.parse(s, DateTimeFormatter.ISO_DATE)))
    p("2021-06-07") shouldBe m.Match(LocalDate.parse("2021-06-07"))
  }
  it should "parserTilde" in {
    val p = m.parserTilde("""(\w+)-(\d+)""")
    p("PG-13") shouldBe m.Match(Tilde("PG", "13"))
  }

  behavior of "parserList"
  it should "parserList 12345" in {
    val p: m.Parser[List[String]] = m.parserList("""(\d+)""")
    p("12345") shouldBe m.Match(List("12345"))
  }
  it should """match (\w+)(\s(\d+))""" in {
    val p: m.Parser[Int] = m.parserGroup("""(\w+)(\s(\d+))""", 3)(m.parserInt)
    p("Hello 12345") shouldBe m.Match(12345)
  }
  it should "parserGroup 12345" in {
    val p: m.Parser[Int] = m.parserGroup("""(\d+)""")(m.parserInt)
    p("12345") shouldBe m.Match(12345)
  }
  it should "parser2 rating with two required parameters" in {
    case class Rating(code: String, age: Int)
    val p: m.Parser[Rating] = m.parser2("""(\w+)-(\d+)""")(m.always, m.parserInt)(Rating)
    p("PG-13") shouldBe m.Match(Rating("PG", 13))
  }
  it should "parser2 rating with one optional parameter" in {
    case class Rating(code: String, age: Option[Int])
    val p: m.Parser[Rating] = m.parser2("""(\w+)-(\d+)?""")(m.always, m.opt(m.parserInt))(Rating)
    p("PG-13") shouldBe m.Match(Rating("PG", Some(13)))
    p("R-") shouldBe m.Match(Rating("R", None))
  }
  it should "parser2 rating with one optional parameter without having to match dash" in {
    case class Rating(code: String, age: Option[Int])
    val p: m.Parser[Rating] = m.parser2("""(\w+)(-(\d+))?""", 1, 3)(m.always, m.opt(m.parserInt))(Rating)
    p("PG-13") shouldBe m.Match(Rating("PG", Some(13)))
    p("R") shouldBe m.Match(Rating("R", None))
  }
  it should "parser3 with three required parameters" in {
    case class Rating(code: String, age: Int, length: Double)
    val p: m.Parser[Rating] = m.parser3("""(\w+)-(\d+) (\d+\.\d+)""")(m.always, m.parserInt, m.parserDouble)(Rating)
    p("PG-13 3.1415927") shouldBe m.Match(Rating("PG", 13, 3.1415927))
  }

  behavior of "~"

  import matchers._

  it should "work" in {
    val m: matchers.Matcher[String ~ String, Int] = "1".m ~ "2".m ^^ {
      case x ~ y => x.toInt + y.toInt
    }
    m("1" ~ "2") shouldBe matchers.Match(3)
  }

  it should "do stuff" in {
    val intParser = m.parserInt
    val z: m.MatchResult[Int ~ Int ~ Int] = intParser("1") ~ intParser("2") ~ intParser("3")
    z.successful shouldBe true
    z.get shouldBe 1 ~ 2 ~ 3
  }

  behavior of "regex"
  it should """miss (\d+) with Hello""" in {
    val m: matchers.Parser[String] = """(\d+)""".regex
    m("Hello") should matchPattern { case matchers.Miss(_, _) => }
  }
  it should """match (\d+) with 12345""" in {
    val m: matchers.Parser[String] = """(\d+)""".regex
    m("12345") shouldBe matchers.Match("12345")
  }

  behavior of "regexGroups"
  it should """miss (\d+) with Hello""" in {
    val m: matchers.Parser[List[String]] = """(\d+)""".regexGroups
    m("Hello") should matchPattern { case matchers.Miss(_, _) => }
  }
  it should """match (\d+) with 12345""" in {
    val m: matchers.Parser[List[String]] = """(\d+)""".regexGroups
    m("12345") shouldBe matchers.Match(List("12345"))
  }
  it should """match (\w+)\s(\d+)""" in {
    val m: matchers.Parser[List[String]] = """(\w+)\s(\d+)""".regexGroups
    m("Hello 12345") shouldBe matchers.Match(List("Hello", "12345"))
  }
  it should """result in an Error for (\d+ with Hello""" in {
    val m: matchers.Parser[List[String]] = """(\d+""".regexGroups
    m("Hello") should matchPattern { case matchers.Error(_) => }
  }

  behavior of "MatchLogger"
  it should "work" in {
    MatchLogger(LogInfo, classOf[MatchersSpec])("Hello")
  }
}

case class SBLogger(override val logLevel: LogLevel, sb: StringBuilder, indent: Int = 0) extends MatchLogger(logLevel, { w => sb.append(s"$w\n"); () }, indent)

