package dotty.tools.dotc

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.reporting.AbstractReporter
import dotty.tools.dotc.reporting.Diagnostic

class ExpressionReporter(reportError: String => Unit) extends AbstractReporter:
  override def doReport(dia: Diagnostic)(using Context): Unit =
    // println(messageAndPos(dia))
    dia match
      case error: Diagnostic.Error =>
        val newPos = error.pos.source.positionInUltimateSource(error.pos)
        val errorWithNewPos = new Diagnostic.Error(error.msg, newPos)
        reportError(stripColor(messageAndPos(errorWithNewPos)))
      case _ =>
        // TODO report the warnings
        ()
