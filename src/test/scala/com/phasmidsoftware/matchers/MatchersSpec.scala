package com.phasmidsoftware.matchers

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
  it should "support Match conditional" in {
    m.Match(0) conditional m.Match("") shouldBe m.Match("")
    m.Match(0) conditional m.Miss("bad", "") shouldBe m.Miss("bad", "")
    m.Match(0) conditional m.Error(new RuntimeException("")) should matchPattern { case m.Error(_: RuntimeException) => }
  }
  it should "support Miss conditional" in {
    m.Miss("bad", 0) conditional m.Match("") shouldBe m.Miss("bad", 0)
    m.Miss("bad", 0) conditional m.Miss("bad", "") shouldBe m.Miss("bad", 0)
    m.Miss("bad", 0) conditional m.Error(new RuntimeException("")) shouldBe m.Miss("bad", 0)
  }
  it should "support Error conditional" in {
    m.Error(new RuntimeException("")) conditional m.Match("") should matchPattern { case m.Error(_: RuntimeException) => }
    m.Error(new RuntimeException("")) conditional m.Miss("bad", "") should matchPattern { case m.Error(_: RuntimeException) => }
    m.Error(new RuntimeException("")) conditional m.Error(new RuntimeException("")) should matchPattern { case m.Error(_: RuntimeException) => }
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
    val result = m.success(0)("") && m.success(1)("")
    result.successful shouldBe true
    result.get shouldBe 0 -> 1
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
  }
  it should "support &" in {
    val result = m.fail("")("") & m.success[Any, Int](0)
    result.successful shouldBe false
  }
  it should "support &&" in {
    val result = m.fail("")("") && m.success(1)("")
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
  it should "support &&" in {
    val result = m.error[Unit](new NoSuchElementException)(()) && m.success(1)("")
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
    val q: m.Matcher[String, Double] = target map (_.toDouble)
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
    val f: m.Matcher[String, Int] = m.Matcher(_ => m.Match(1))
    f("1").successful shouldBe true
  }
  it should "work with fixed fail result" in {
    val f: m.Matcher[String, Int] = m.Matcher(e => m.Miss("", e))
    f("1").successful shouldBe false
  }
  it should "result in error when function throws exception" in {
    def f(x: String): m.MatchResult[Int] = m.Match(x.toInt)

    val p: m.Matcher[String, Int] = m.Matcher(f)
    val result = p("x")
    result.successful shouldBe false
    result should matchPattern { case m.Error(_) => }
    a[NumberFormatException] shouldBe thrownBy(result.getOrElse(""))
  }

  behavior of "log"
  it should "log success with LogDebug" in {
    val sb = new StringBuilder
    implicit val ll: LogLevel = LogDebug
    implicit val logger: MatchLogger = { w => sb.append(s"$w\n"); () }
    import m.MatcherOps
    val p = m.success(1) :| "success(1)"
    p(1).successful shouldBe true
    sb.toString() shouldBe
      """trying success(1) on 1...
        |... success(1): Match: 1
        |""".stripMargin
  }
  it should "log success with LogInfo" in {
    val sb = new StringBuilder
    import m.MatcherOps
    implicit val ll: LogLevel = LogInfo
    implicit val logger: MatchLogger = { w => sb.append(s"$w\n"); () }
    val p = m.success(1) :| "success(1)"
    p(1).successful shouldBe true
    sb.toString() shouldBe
            """success(1): matched 1
              |""".stripMargin
  }
  it should "log success with LogOff" in {
    val sb = new StringBuilder
    import m.MatcherOps
    implicit val ll: LogLevel = LogOff
    implicit val logger: MatchLogger = { w => sb.append(s"$w\n"); () }
    val p = m.success(1) :| "success(1)"
    p(1).successful shouldBe true
    sb.toString() shouldBe ""
  }

  // CONSIDER eliminating the namedMatcher method.
  behavior of "namedMatcher"
  it should "work with fixed success result" in {
    val sb = new StringBuilder
    implicit val ll: LogLevel = LogDebug
    implicit val logger: MatchLogger = { w => sb.append(s"$w\n"); () }
    val f: m.Matcher[String, Int] = m.namedMatcher("one")(_ => m.Match(1))
    f("1").successful shouldBe true
    sb.toString() shouldBe
      """trying one on 1...
              |... one: Match: 1
              |""".stripMargin
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
    val z: m.Matcher[(Int, String), Int] = m.valve(_.toInt, p)
    z(1, "1").successful shouldBe true
  }
  it should "match parity" in {
    val p: (Int, Int) => Boolean = {
      case (x, y) => y % 2 == x
    }
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
    val t = (1, "1")
    val u: m.MatchResult[(String, Int)] = m.swap(t)
    u shouldBe m.Match("1", 1)
  }

  behavior of "*"
  it should "work" in {
    val t = (1, 2)
    val p: m.Matcher[(Int, Int), (Int, Int)] = m.filter2_0(m.matches(2))
    p(t).successful shouldBe false
    m.*(p)(t).successful shouldBe true
  }

  behavior of "**"
  it should "work" in {
    val t = (1, 2, 3)
    val p: m.Matcher[(Int, Int, Int), (Int, Int, Int)] = m.filter3_0(m.matches(2))
    p(t).successful shouldBe false
    m.**(p)(t).successful shouldBe true
  }

  behavior of "filter"
  it should "filter2_0" in {
    val t = (1, 2)
    val p1: m.Matcher[(Int, Int), (Int, Int)] = m.filter2_0(m.matches(2))
    p1(t).successful shouldBe false
    val p2: m.Matcher[(Int, Int), (Int, Int)] = m.filter2_0(m.matches(1))
    p2(t).successful shouldBe true
  }
  it should "filter2_1" in {
    val t = ("1", 2)
    val p1: m.Matcher[(String, Int), (String, Int)] = m.filter2_1(m.matches(2))
    p1(t).successful shouldBe true
    val p2: m.Matcher[(String, Int), (String, Int)] = m.filter2_1(m.matches(1))
    p2(t).successful shouldBe false
  }
  it should "filter3_0" in {
    val t = ("1", 2, 3.0)
    val p1: m.Matcher[(String, Int, Double), (String, Int, Double)] = m.filter3_0(m.matches("1"))
    p1(t).successful shouldBe true
    val p2: m.Matcher[(String, Int, Double), (String, Int, Double)] = m.filter3_0(m.matches("2"))
    p2(t).successful shouldBe false
  }
  it should "filter3_1" in {
    val t = ("1", 2, 3.0)
    val p1: m.Matcher[(String, Int, Double), (String, Int, Double)] = m.filter3_1(m.matches(2))
    p1(t).successful shouldBe true
    val p2: m.Matcher[(String, Int, Double), (String, Int, Double)] = m.filter3_1(m.matches(1))
    p2(t).successful shouldBe false
  }
  it should "filter3_2" in {
    val t = ("1", 2, 3.0)
    val p1: m.Matcher[(String, Int, Double), (String, Int, Double)] = m.filter3_2(m.matches(3.0))
    p1(t).successful shouldBe true
    val p2: m.Matcher[(String, Int, Double), (String, Int, Double)] = m.filter3_2(m.matches(0))
    p2(t).successful shouldBe false
  }

  behavior of "rotate3"
  it should "work" in {
    val t = (1, "1", 1.0)
    val u: m.MatchResult[(String, Double, Int)] = m.rotate3(t)
    u shouldBe m.Match("1", 1.0, 1)
  }

  behavior of "invert3"
  it should "work" in {
    val t = (1, "1", 1.0)
    val u: m.MatchResult[(Double, String, Int)] = m.invert3(t)
    u shouldBe m.Match(1.0, "1", 1)
  }

  behavior of "~"
  it should "match (1,2) and result in (1,2)" in {
    val p = m.matches(1)
    val q = m.matches(2)
    val z = p ~ q
    val result = z(1 -> 2)
    result.successful shouldBe true
    result.get shouldBe Tilde(1, 2)
  }
  it should "match (1,2) and result in 2" in {
    val p = m.matches(1)
    val q = m.matches(2)
    val z = p ~> q
    val result = z(1 -> 2)
    result.successful shouldBe true
    result.getOrElse(0) shouldBe 2
  }
  it should "match (1,2) and result in 1" in {
    val p = m.matches(1)
    val q = m.matches(2)
    val z = p <~ q
    val result = z(1 -> 2)
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
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail("")
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail("")
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(q, p)(StringPair)
    val tuple = StringPair("", "1")
    r(tuple).successful shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail("")
    val q: m.Matcher[String, Int] = m.fail("")
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe false
  }

  behavior of "matchProduct2All"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val r: m.Matcher[StringPair, (Int, Int)] = m.matchProduct2All(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }

  behavior of "matchProduct3All"

  case class Triple(t1: String, t2: String, t3: Int)

  it should "succeed with toInt and 0 and identity" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val z: m.Matcher[Int, Int] = m.lift(identity)
    val r: m.Matcher[Triple, (Int, Int, Int)] = m.matchProduct3All(p, q, z)(Triple.apply)
    val tuple = Triple("1", "", 0)
    r(tuple).successful shouldBe true
  }

  behavior of "flip"
  it should "work" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[Int, Double] = m.lift(_.toDouble)
    val z: m.Matcher[(String, Int), Int ~ Double] = p ~ q
    z("1", 1) should matchPattern { case m.Match(~(1, 1.0)) => }
    m.flip(z)(1, "1") should matchPattern { case m.Match(~(1, 1.0)) => }
  }

  behavior of "tuple2"
  it should "work" in {
    val p: m.Matcher[StringPair, (String, String)] = m.tuple2(StringPair)
    p(StringPair("x", "y")) should matchPattern { case m.Match(("x", "y")) => }
  }

  behavior of "tuple3"
  it should "work" in {
    val p: m.Matcher[Triple, (String, String, Int)] = m.tuple3(Triple)
    p(Triple("x", "y", 1)) should matchPattern { case m.Match(("x", "y", 1)) => }
  }

  behavior of "product2"
  it should "one-way" in {
    val p: m.Matcher[(String, String), StringPair] = m.product2(StringPair)
    p(("x", "y")) should matchPattern { case m.Match(StringPair("x", "y")) => }
  }
  it should "round-trip" in {
    val p: m.Matcher[StringPair, StringPair] = m.tuple2(StringPair) & m.product2(StringPair)
    p(StringPair("x", "y")) should matchPattern { case m.Match(StringPair("x", "y")) => }
  }

  behavior of "product3"
  it should "one-way" in {
    val p: m.Matcher[(String, String, Int), Triple] = m.product3(Triple)
    p(("x", "y", 1)) should matchPattern { case m.Match(Triple("x", "y", 1)) => }
  }
  it should "round-trip" in {
    val p: m.Matcher[Triple, Triple] = m.tuple3(Triple) & m.product3(Triple)
    p(Triple("x", "y", 1)) should matchPattern { case m.Match(Triple("x", "y", 1)) => }
  }

  behavior of "match2Any"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val r: m.Matcher[(String, String), Int] = m.match2Any(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail("")
    val r: m.Matcher[(String, String), Int] = m.match2Any(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail("")
    val r: m.Matcher[(String, String), Int] = m.match2Any(q, p)
    val tuple = ("", "1")
    r(tuple).successful shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail("")
    val q: m.Matcher[String, Int] = m.fail("")
    val r: m.Matcher[(String, String), Int] = m.match2Any(q, p)
    val tuple = ("1", "")
    r(tuple).successful shouldBe false
  }

  behavior of "match3Any"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail("")
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail("")
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail("")
    val q: m.Matcher[String, Int] = m.fail("")
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    val result = r(tuple)
    result should matchPattern { case m.Error(_: ArithmeticException) => }
  }

  behavior of "match2All"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val r: m.Matcher[(String, String), Int ~ Int] = m.match2All(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe true
  }
  it should "fail with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail("")
    val r: m.Matcher[(String, String), Int ~ Int] = m.match2All(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe false
  }
  it should "fail with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail("")
    val r: m.Matcher[(String, String), Int ~ Int] = m.match2All(q, p)
    val tuple = ("", "1")
    r(tuple).successful shouldBe false
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail("")
    val q: m.Matcher[String, Int] = m.fail("")
    val r: m.Matcher[(String, String), Int ~ Int] = m.match2All(q, p)
    val tuple = ("", "1")
    r(tuple).successful shouldBe false
  }

  behavior of "match3All"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val z: m.Matcher[String, Int] = m.success(1)
    val r: m.Matcher[(String, String, String), (Int, Int, Int)] = m.match3All(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "fail with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail("")
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), (Int, Int, Int)] = m.match3All(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe false
  }

  behavior of "opt"
  it should "succeed with a match" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q = m.opt(p)
    q("1") should matchPattern { case m.Match(Some(1)) => }
  }

  behavior of "?"
  it should "succeed with a match" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Option[Int]] = p.?
    q("1") should matchPattern { case m.Match(Some(1)) => }
  }

  behavior of "trial"
  it should "succeed with a match" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Try[Int]] = p.trial
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

  behavior of "~"
  it should "work" in {
    import matchers._
    val m: matchers.Matcher[(String, String), Int] = "1".m ~ "2".m ^^ {
      case x ~ y => x.toInt + y.toInt
    }
    m(("1", "2")) shouldBe matchers.Match(3)
  }

  behavior of "regex"
  it should "work" in {
    import matchers._
    val m: matchers.Matcher[String, String] = """(\d+)""".regex
    m("12345") shouldBe matchers.Match("12345")
  }
}
