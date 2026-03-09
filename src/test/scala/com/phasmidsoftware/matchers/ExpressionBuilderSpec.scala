package com.phasmidsoftware.matchers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionBuilderSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ExpressionBuilder"

  it should "do Claude's test 1" in {
    val result: Expression = ∅ + 1 + 2
    result shouldBe an[Expression]
    result.value shouldBe 3
  }
  it should "do Claude's test 2" in {
    val result = 2 * 𝛑
    result shouldBe an[Expression]
    result.value shouldBe -2
  }

  it should "create zero Expression without using ∅" in {
    val result: Expression = 0
    result shouldBe an[Expression]
    result.value shouldBe 0
  }
  it should "create zero Expression using ∅" in {
    val result = ∅ + 0
    result shouldBe an[Expression]
    result.value shouldBe 0
  }

  it should "create unit Expression" in {
    val result = ∅ + 1
    result shouldBe an[Expression]
    result.value shouldBe 1
  }

  it should "create Expression Expression" in {
    val result = ∅ + Valuable(1)
    result shouldBe an[Expression]
    result.value shouldBe 1
  }

  it should "create String Expression" in {
    val result = ∅ + "1" + 2
    result shouldBe an[Expression]
    result.value shouldBe 3
  }
}
