package ch.epfl.scala.debugadapter.internal.stacktrace

import com.sun.jdi
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedVariable

final case class JavaVariable(variable: jdi.LocalVariable) extends DecodedVariable {
  def format: String = {
    variable.name()
  }
}
