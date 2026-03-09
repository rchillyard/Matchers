package com.phasmidsoftware.matchers

import org.slf4j.Logger
import scala.util.matching.Regex

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
  /**
    * Implicit value representing the log-off level.
    * Used as a default log level to disable logging where a `LogLevel`
    * is required implicitly within the application.
    */
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
      case LogInfo | LogDebug =>
        f(w)
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
    case LogInfo =>
      logger.info(w)
    case LogDebug =>
      logger.debug(w)
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
  def apply(logLevel: LogLevel, clazz: Class[?]): MatchLogger =
    Slf4jLogger(logLevel, LoggerFactory.getLogger(clazz))

  /**
    * Creates a `MatchLogger` instance with the `LogInfo` log level
    * using the provided SLF4J `Logger`.
    *
    * @param logger The SLF4J `Logger` to be used for logging.
    * @return A `MatchLogger` instance configured to log at the `LogInfo` level and
    *         utilize the given SLF4J `Logger` for logging operations.
    */
  def apply(logger: Logger): MatchLogger =
    Slf4jLogger(LogInfo, logger)
}
