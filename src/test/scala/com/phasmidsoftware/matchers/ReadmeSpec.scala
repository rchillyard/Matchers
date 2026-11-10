package com.phasmidsoftware.matchers

import com.phasmidsoftware.matchers.Matchers.matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * Checks every example in README.md, including the output each one is shown as producing.
  *
  * The README quotes results such as "Match: 4", so these tests assert on `toString` as well as on
  * the value. That is deliberate: if a rendering changes, the README is stale and should be updated
  * along with the code. If a test here fails, fix the README too.
  *
  * Each test names the README section it comes from.
  */
class ReadmeSpec extends AnyFlatSpec with should.Matchers {

  private val m = matchers

  // The two matchers that the README's "Your first matcher" and "Composition of MatchResult"
  // sections share.
  private val even: m.Matcher[Int, Int] = m.filter[Int](_ % 2 == 0)
  private val double: m.Matcher[Int, Int] = m.lift[Int, Int](_ * 2)

  behavior of "README: Your first matcher"

  it should "match an even number" in {
    even(4) shouldBe m.Match(4)
    even(4).toString shouldBe "Match: 4"
  }
  it should "miss an odd number" in {
    even(3).successful shouldBe false
    even(3).toString shouldBe "Miss: filter: predicate returned false for 3: 3"
  }

  behavior of "README: Creating a Matchers instance"

  it should "support an instance of one's own" in {
    MyMatchers.matches(1)(1).toString shouldBe "Match: 1"
  }
  it should "support extending the trait with one's own matchers" in {
    StringMatchers.nonEmpty("hi").toString shouldBe "Match: hi"
    StringMatchers.nonEmpty("").toString shouldBe "Miss: filter: predicate returned false for : "
  }

  behavior of "README: Composition of MatchResult"

  it should "treat && as a guard which discards the left-hand value" in {
    (even(4) && double(21)).toString shouldBe "Match: 42"
    (even(3) && double(21)).toString shouldBe "Miss: filter: predicate returned false for 3: 3"
  }
  it should "treat ~ as the operator which keeps both values" in {
    (even(4) ~ double(21)).toString shouldBe "Match: 4~42"
  }
  it should "transform a result with ^^" in {
    val asString: m.Matcher[Int, String] = even ^^ (x => s"<$x>")
    asString(4).toString shouldBe "Match: <4>"
  }
  it should "choose between alternatives with |" in {
    val oneOrTwo: m.Matcher[Int, Int] = m.matches(1) | m.matches(2)
    oneOrTwo(2).toString shouldBe "Match: 2"
    oneOrTwo(3).successful shouldBe false
  }

  behavior of "README: Parsing"

  it should "parse an Int" in {
    val p = m.parserInt
    p("12345").toString shouldBe "Match: 12345"
    p("abc").toString shouldBe """Miss: parser: String abc did not match regex -?\d+: abc"""
  }
  it should "parse regex groups" in {
    import m.*
    val rg: m.Parser[List[String]] = """(\w+)\s(\d+)""".regexGroups
    rg("Hello 12345").toString shouldBe "Match: List(Hello, 12345)"
  }
  it should "parse into a case class, with an optional group" in {
    val pr: m.Parser[Rating] =
      m.parser2("""(\w+)(-(\d+))?""", 1, 3)(m.always, m.opt(m.parserInt))(Rating.apply)
    pr("PG-13").toString shouldBe "Match: Rating(PG,Some(13))"
    pr("R").toString shouldBe "Match: Rating(R,None)"
  }

  behavior of "README: Tilde"

  it should "combine two String matchers and map the result" in {
    import m.*
    import com.phasmidsoftware.matchers.Matchers.TildeOps

    val t: m.Matcher[String ~ String, Int] = "1".m ~ "2".m ^^ {
      case x ~ y => x.toInt + y.toInt
    }
    t("1" ~ "2").toString shouldBe "Match: 3"
  }

  behavior of "README: Debugging/Logging"

  it should "name a matcher with :|" in {
    import m.MatcherOps
    val p = m.success(1) :| "success(1)"
    p(()).toString shouldBe "Match: 1"
  }
}

/**
  * Used by the "Parsing" example. A case class nested in the spec could not be rendered as
  * "Rating(PG,Some(13))".
  */
case class Rating(code: String, age: Option[Int])

/** The README's "Creating a Matchers instance", route 2. */
object MyMatchers extends Matchers {
  protected val matchLogger: MatchLogger = MatchLogger(LogOff, classOf[Matchers])
}

/** The README's "Creating a Matchers instance", route 3. */
trait StringMatchers extends Matchers {
  def nonEmpty: Matcher[String, String] = filter[String](_.nonEmpty)
}

object StringMatchers extends StringMatchers {
  protected val matchLogger: MatchLogger = MatchLogger(LogOff, classOf[Matchers])
}
