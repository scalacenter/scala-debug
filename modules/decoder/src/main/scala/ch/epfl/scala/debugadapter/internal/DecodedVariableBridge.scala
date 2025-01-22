package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.decoder.DecodedVariable
import ch.epfl.scala.decoder.StackTraceFormatter
import tastyquery.Symbols.*
import tastyquery.Modifiers.*
import ch.epfl.scala.decoder.DecodedVariable.ValDef
import ch.epfl.scala.decoder.DecodedVariable.CapturedVariable
import ch.epfl.scala.decoder.DecodedVariable.This
import ch.epfl.scala.decoder.DecodedVariable.AnyValThis

class DecodedVariableBridge(variable: DecodedVariable):
  def format: String = variable match
    case v: ValDef => v.symbol.name.toString
    case v: CapturedVariable => v.symbol.name.toString
    case v: This => "this"
    case v: AnyValThis => v.symbol.name.toString

    // formatter.format(variable)
