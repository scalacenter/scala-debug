package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.ByteCode
import ch.epfl.scala.debugadapter.internal.ThrowOrWarn
import ch.epfl.scala.debugadapter.internal.stacktrace.JdiExtensions.*
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedMethod
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedVariable
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedField
import com.sun.jdi

import scala.util.control.NonFatal

class Scala3Decoder(
    debuggee: Debuggee,
    classLoader: ClassLoader,
    private var bridge: Scala3DecoderBridge,
    protected val logger: Logger,
    protected val testMode: Boolean
) extends ScalaDecoder
    with ThrowOrWarn {

  override def skipOver(method: jdi.Method): Boolean =
    // some trait initializers are completely empty
    if (method.isTraitInitializer) method.bytecodes.toSeq == Seq(ByteCode.RETURN)
    else decode(method).isGenerated

  override def decode(method: jdi.Method): DecodedMethod =
    try
      if (method.declaringType.isDynamicClass) JavaMethod(method, isGenerated = true)
      else if (method.isJava)
        JavaMethod(method, isGenerated = (method.isBridge || method.isSynthetic) && !method.isConstructor)
      else if (method.isStaticMain) {
        // Scala static main methods don't contain any debug info
        // also we pretend it is not generated to avoid empty stack traces
        JavaMethod(method, isGenerated = false)
      } else if (method.isStaticConstructor) {
        // try remove and fix binary decoder
        JavaMethod(method, isGenerated = false)
      } else bridge.decode(method)
    catch {
      case NonFatal(e) =>
        throwOrWarn(e)
        JavaMethod(method, isGenerated = method.isBridge)
    }

  override def decode(variable: jdi.LocalVariable, method: jdi.Method, sourceLine: Int): DecodedVariable =
    try
      if (method.declaringType().isDynamicClass) {
        JavaVariable(variable)
      } else if (method.isJava) {
        JavaVariable(variable)
      } else if (method.isStaticMain || method.isStaticConstructor) {
        JavaVariable(variable)
      } else bridge.decode(variable, method, sourceLine)
    catch {
      case NonFatal(e) =>
        throwOrWarn(e)
        JavaVariable(variable)
    }

  override def decode(field: jdi.Field): DecodedField =
    try bridge.decode(field)
    catch {
      case NonFatal(e) =>
        throwOrWarn(e)
        JavaField(field)
    }

  override def reload(): Unit =
    bridge = Scala3DecoderBridge(debuggee, classLoader, logger, testMode)
}

object Scala3Decoder {
  def apply(debuggee: Debuggee, classLoader: ClassLoader, logger: Logger, testMode: Boolean): Scala3Decoder = {
    val bridge = Scala3DecoderBridge(debuggee, classLoader, logger, testMode)
    new Scala3Decoder(debuggee, classLoader, bridge, logger, testMode)
  }
}
