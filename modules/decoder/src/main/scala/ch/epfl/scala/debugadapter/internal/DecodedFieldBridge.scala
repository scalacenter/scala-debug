package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.decoder.DecodedField
import ch.epfl.scala.decoder.StackTraceFormatter
import tastyquery.Symbols.*
import tastyquery.Modifiers.*
import tastyquery.Names.*
import ch.epfl.scala.decoder.DecodedField.ValDef
import ch.epfl.scala.decoder.DecodedField.ModuleVal
import ch.epfl.scala.decoder.DecodedField.LazyValOffset
import ch.epfl.scala.decoder.DecodedField.Outer
import ch.epfl.scala.decoder.DecodedField.Capture
import ch.epfl.scala.decoder.DecodedField.LazyValBitmap

class DecodedFieldBridge(field: DecodedField):
  def format: String = field match
    case field: ValDef => format(field.symbol.name)
    case field: ModuleVal => ""
    case field: LazyValOffset => ""
    case field: Outer => "<outer>"
    case field: ch.epfl.scala.decoder.DecodedField.SerialVersionUID => ""
    case field: Capture => field.symbol.name.toString()
    case field: LazyValBitmap => ""

  def show: Boolean = true

  // formatter.format(variable)
  private def format(name: Name): String =
    def rec(name: Name): String = name match
      case DefaultGetterName(termName, num) => s"${termName.toString()}.<default ${num + 1}>"
      case name: TypeName => rec(name.toTermName)
      case SimpleName("$anonfun") => "<anon fun>"
      case SimpleName("$anon") => "<anon class>"
      case ObjectClassName(underlying) => rec(underlying)
      case UniqueName(SimpleName(""), _, _) => "<anon>"
      case _ => name.toString
    rec(name)
