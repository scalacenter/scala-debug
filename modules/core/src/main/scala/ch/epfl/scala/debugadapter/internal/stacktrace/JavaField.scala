package ch.epfl.scala.debugadapter.internal.stacktrace

import com.sun.jdi
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedField

final case class JavaField(field: jdi.Field) extends DecodedField {

  override def show(): Boolean =
    !field.isStatic() || field.declaringType.name.endsWith("$")

  def format: String = {
    field.name()
  }
}
