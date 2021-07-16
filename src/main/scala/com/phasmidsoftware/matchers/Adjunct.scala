package com.phasmidsoftware.matchers

import scala.util.matching.Regex

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
  def disabled: Boolean = logLevel == LogOff

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

  implicit val matchLogger: MatchLogger = apply(LogOff, classOf[Matchers])

  import org.slf4j.LoggerFactory

  def apply(logLevel: LogLevel, clazz: Class[_]): MatchLogger = Slf4jLogger(logLevel, LoggerFactory.getLogger(clazz))

  def apply(logger: Logger): MatchLogger = Slf4jLogger(LogInfo, logger)
}
