package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.Errors
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedVariable

import java.lang.reflect.InvocationTargetException

class DecodedVariableBridge(instance: Any) extends DecodedVariable {

  override def format(): String = invoke[String]("format")

  private def invoke[T](methodName: String): T =
    try instance.getClass.getMethod(methodName).invoke(instance).asInstanceOf[T]
    catch {
      case e: InvocationTargetException =>
        throw Errors.frameDecodingFailure(e.getCause)
    }
}
