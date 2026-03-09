package com.phasmidsoftware.matchers

import scala.util.matching.Regex

/**
  * A class combining a regular expression and a list of group indexes.
  * NOTE: there are no unit tests for this class.
  *
  * @param regex  a Regex.
  * @param groups a list of required group indexes (if Nil, then all groups are selected).
  *               NOTE that the group indexes start at 1.
  */
case class RegexGroups(regex: Regex, groups: Seq[Int]) {
  /**
    * Attempts to match a given `CharSequence` against the regular expression
    * and extract the selected groups if the match is successful.
    *
    * @param s the input `CharSequence` to be matched against the regular expression.
    * @return an `Option` containing a `List` of the selected group strings
    *         if the match is successful, or `None` if no match is found.
    */
  def unapplySeq(s: CharSequence): Option[List[String]] =
    regex.unapplySeq(s) map selectGroups

  /**
    * Selects a subset of strings from the provided list based on the pre-defined group indexes.
    * If no group indexes are defined (i.e., the group list is empty), all elements in the input list are returned.
    *
    * @param ws the input list of strings to select groups from.
    * @return a list of strings corresponding to the selected group indexes, or all strings when no group indexes are specified.
    */
  private def selectGroups(ws: List[String]): List[String] = groups match {
    case Nil =>
      ws
    case groups =>
      (for group <- groups yield ws(group - 1)).toList
  }
}

/**
  * An object providing factory methods for creating instances of RegexGroups,
  * which represent a combination of a regular expression and a list of group indexes.
  *
  * It includes methods to construct RegexGroups with a regex pattern as a `Regex` instance
  * or as a string, along with an optional list of group indexes.
  * If no group indexes are provided, all groups are selected by default.
  */
object RegexGroups {
  /**
    * Constructs a `RegexGroups` instance using the provided regular expression and no specific group indexes.
    * If no group indexes are specified, all groups are selected by default.
    *
    * @param regex the regular expression to be used for matching.
    * @return a `RegexGroups` instance combining the provided regular expression and default group indexes.
    */
  def apply(regex: Regex): RegexGroups = apply(regex, Nil)

  /**
    * Constructs a `RegexGroups` instance using the provided regular expression string and no specific group indexes.
    * If no group indexes are specified, all groups are selected by default.
    *
    * @param regex the regular expression string to be used for matching.
    * @return a `RegexGroups` instance combining the provided regular expression and default group indexes.
    */
  def apply(regex: String): RegexGroups = apply(new Regex(regex))

  /**
    * Creates a `RegexGroups` instance by combining the provided regular expression
    * with the specified group indexes.
    *
    * @param regex  the regular expression to be used for matching.
    * @param groups the group indexes to be extracted from the regular expression.
    *               If no indexes are provided, all groups will be selected.
    * @return a `RegexGroups` instance containing the regular expression and the list of specified group indexes.
    */
  def create(regex: Regex, groups: Int*): RegexGroups = apply(regex, groups)

  /**
    * Creates an instance of `RegexGroups` by combining a regular expression pattern,
    * supplied as a string, with a sequence of group indexes.
    *
    * @param regex  the regular expression pattern as a string.
    * @param groups a variable-length sequence of integers specifying the group indexes to include.
    *               If not specified, all groups will be included by default.
    * @return a `RegexGroups` instance that combines the given regex pattern and the specified group indexes.
    */
  def create(regex: String, groups: Int*): RegexGroups = apply(new Regex(regex), groups)
}

/**
  * The tilde class which is basically a Tuple (i.e., Product) with a few extra methods.
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

  /**
    * Returns a string representation of this instance.
    *
    * @return the string representation combining the left and right values with a tilde (~) in between.
    */
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

/**
  * Companion object for the MatcherException case class.
  * Provides utility methods for creating MatcherException instances.
  */
object MatcherException {
  /**
    * Creates a new instance of the MatcherException class with the specified message.
    *
    * @param msg the detail message for the exception
    * @return a new MatcherException instance with the given message and a null cause
    */
  def apply(msg: String): MatcherException = MatcherException(msg, null)
}
