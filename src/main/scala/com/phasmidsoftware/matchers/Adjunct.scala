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
  def unapplySeq(s: CharSequence): Option[List[String]] = regex.unapplySeq(s) map selectGroups

  private def selectGroups(ws: List[String]): List[String] = groups match {
    case Nil => ws
    case groups => (for (group <- groups) yield ws(group - 1)).toList
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

import org.slf4j.Logger

/**
  * Trait which is used to define a logging level for the log method of SignificantSpaceParsers.
  */
trait LogLevel

/**
  * Represents a logging level for debugging purposes.
  *
  * This object is a case object that extends the LogLevel trait.
  * It is used to specify that log messages should correspond
  * to the debug logging level.
  */
case object LogDebug extends LogLevel

/**
  * Represents the information level for logging within the context of `SignificantSpaceParsers`.
  * This level is used to log informational messages that provide general insights or details
  * about the execution without indicating an error or warning.
  */
case object LogInfo extends LogLevel

/**
  * Represents a log-off level used to disable logging.
  * Extends the LogLevel trait, indicating it is one of the possible
  * log level settings.
  * Typically used to silence all log messages.
  */
case object LogOff extends LogLevel

/**
  * LogLevel is an object that provides a default implicit value representing
  * the log-off level.
  * This implicit value can be used to disable logging
  * when a LogLevel is required implicitly within the application.
  *
  * LogLevel represents a companion to the LogLevel trait,
  * which defines different logging levels.
  */
object LogLevel {
  implicit val ll: LogLevel = LogOff
}

/**
  * A logger class for handling and managing logging operations
  * based on the specified log level.
  *
  * @constructor Creates a new MatchLogger instance with a given log level and a logging function.
  * @param logLevel The logging level to determine the behavior of the logger (LogOff, LogInfo, LogDebug).
  * @param f        A function that takes a string input and performs the logging operation.
  * @note The class extends `String => Unit`, enabling it to be used directly as a function.
  */
class MatchLogger(val logLevel: LogLevel, f: String => Unit) extends ((String => Unit)) {
  /**
    * Indicates whether the logger is disabled.
    *
    * @return True if the logging level is `LogOff`, meaning no logging will occur;
    *         otherwise, false.
    */
  def disabled: Boolean = logLevel == LogOff

  /**
    * Applies the provided logging function if the logging level is `LogInfo` or `LogDebug`.
    * The log action is skipped if the logging level does not match these criteria.
    *
    * @param w The input string to be logged by the provided logging function.
    * @return Unit value indicating the completion of the logging action or its omission
    *         based on the log level.
    */
  override def apply(w: String): Unit =
    logLevel match {
      case LogInfo | LogDebug => f(w)
      case _ =>
    }
}

/**
  * A case class that wraps SLF4J logging functionality, extending `MatchLogger` to utilize
  * a specified logging level and SLF4J logger instance for log operations.
  *
  * @param logLevel The logging level to control the behavior of the logger, determining
  *                 whether logs are written based on the specified level (e.g., `LogInfo`, `LogDebug`).
  * @param logger   The SLF4J `Logger` instance used for performing the logging operations.
  *
  *                 The class delegates the actual logging operations to SLF4J's `Logger` methods based
  *                 on the provided logging level:
  *                 - Logs at the `info` level if the `logLevel` is `LogInfo`.
  *                 - Logs at the `debug` level if the `logLevel` is `LogDebug`.
  *                 - Skips logging for other levels.
  *
  *                 Inherits functionality from `MatchLogger`, allowing it to act as a functional logger
  *                 with behavior controlled by the specified `logLevel`.
  */
case class Slf4jLogger(override val logLevel: LogLevel, logger: Logger) extends MatchLogger(logLevel, w => {
  logLevel match {
    case LogInfo => logger.info(w)
    case LogDebug => logger.debug(w)
    case _ =>
  }
})

/**
  * Companion object for the `MatchLogger` class, providing utility methods
  * to create instances of `MatchLogger` using standard configurations.
  *
  * This object facilitates the creation of `MatchLogger` instances
  * with specified log levels, target classes, or custom logger implementations.
  */
object MatchLogger {

  /**
    * An implicit `MatchLogger` instance configured with `LogOff` log level
    * and associated with the `Matchers` class.
    *
    * This logger is used implicitly wherever a `MatchLogger` is required,
    * enabling logging behavior to be defined according to the specified
    * `LogOff` level.
    * With `LogOff`, logging is effectively disabled.
    *
    * The logger is instantiated using the `apply` method of the `MatchLogger`
    * companion object.
    */
  implicit val matchLogger: MatchLogger = apply(LogOff, classOf[Matchers])

  import org.slf4j.LoggerFactory

  /**
    * Creates a new instance of `MatchLogger` with the specified logging level
    * and the logger associated with the given class.
    *
    * This method utilizes the SLF4J LoggerFactory to generate the underlying logger
    * instance for the provided class and combines it with the specified log level
    * to create a `MatchLogger`.
    *
    * @param logLevel The desired logging level for the `MatchLogger` (e.g., LogOff, LogInfo, LogDebug).
    * @param clazz    The class for which the logger will be created, linking log messages to it.
    * @return A `MatchLogger` instance configured with the given log level and class-based logger.
    */
  def apply(logLevel: LogLevel, clazz: Class[_]): MatchLogger = Slf4jLogger(logLevel, LoggerFactory.getLogger(clazz))

  /**
    * Creates a `MatchLogger` instance with the `LogInfo` log level
    * using the provided SLF4J `Logger`.
    *
    * @param logger The SLF4J `Logger` to be used for logging.
    * @return A `MatchLogger` instance configured to log at the `LogInfo` level and
    *         utilize the given SLF4J `Logger` for logging operations.
    */
  def apply(logger: Logger): MatchLogger = Slf4jLogger(LogInfo, logger)
}
