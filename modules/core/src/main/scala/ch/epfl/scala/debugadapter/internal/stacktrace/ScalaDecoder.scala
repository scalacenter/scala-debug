package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Logger

import ch.epfl.scala.debugadapter.internal.DebugTools
import ch.epfl.scala.debugadapter.internal.ScalaExtension.*
import com.sun.jdi
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedMethod
import scala.util.Try
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedVariable
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedField

trait ScalaDecoder extends StepFilter {
  def decode(method: jdi.Method): DecodedMethod
  def decode(variable: jdi.LocalVariable, method: jdi.Method, sourceLine: Int): DecodedVariable
  def decode(field: jdi.Field): DecodedField
  def reload(): Unit
}

object ScalaDecoder {
  def apply(
      debuggee: Debuggee,
      tools: DebugTools,
      logger: Logger,
      testMode: Boolean
  ): ScalaDecoder = {
    if (debuggee.scalaVersion.isScala2)
      new Scala2Decoder(tools.sourceLookUp, debuggee.scalaVersion, logger, testMode)
    else
      tools.decoder
        .flatMap { classLoader =>
          Try(Scala3Decoder(debuggee, classLoader, logger, testMode))
            .warnFailure(logger, s"Cannot load step filter for Scala ${debuggee.scalaVersion}")
        }
        .getOrElse(new Scala2Decoder(tools.sourceLookUp, debuggee.scalaVersion, logger, testMode))
  }
}
