package com.phasmidsoftware.matchers

/**
  * The `∅` object represents an empty `Expression`.
  * It is intended to be used as a placeholder for an expression anywhere that the context does not explicity specify Expression.
  * It provides functionality to perform addition operations (`+`) with various operand types,
  * specifically with another `Expression`, Int, or a String representing a numeric value.
  */
private[matchers] object ∅ {
  /**
    * Combines this `Expression` with the given `Expression` using the addition (+) operator.
    *
    * @param operand the `Expression` to be added to this `Expression`.
    * @return an `Expression` representing the sum of this `Expression` and the given `Expression`.
    */
  def +(operand: Expression): Expression = operand

  /**
    * Creates a new `Expression` by adding the specified integer value to this operator.
    *
    * @param operand the integer value to be added.
    * @return a new instance of `Expression` representing the sum.
    */
  def +(operand: Int): Expression = Valuable(operand)

  /**
    * Adds the given string operand, converting it to an integer before performing the operation.
    *
    * @param operand the string representation of an integer to be added to the expression.
    * @return a new `Expression` representing the addition of this expression and the integer value of the given string.
    */
  def +(operand: String): Expression = Valuable(operand.toInt)
}
