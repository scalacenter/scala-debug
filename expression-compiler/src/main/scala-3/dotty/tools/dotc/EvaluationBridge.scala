package dotty.tools.dotc

import java.nio.file.Path
import java.util.function.Consumer
import java.{util => ju}
import collection.JavaConverters._
import scala.util.control.NonFatal
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.core.Contexts.Context

class EvaluationBridge:
  def run(
      expressionDir: Path,
      expressionClassName: String,
      classPath: String,
      sourceFile: Path,
      line: Int,
      expression: String,
      localVariables: ju.Set[String],
      pckg: String,
      errorConsumer: Consumer[String],
      timeoutMillis: Long
  ): Boolean =
    val args = Array(
      "-d",
      expressionDir.toString,
      "-classpath",
      classPath,
      "-Yskip:pureStats",
      // Debugging: Print the tree after each phases of the debugger
      // "-Vprint:extract-expression,resolve-reflect-eval",
      sourceFile.toString
    )
    val evalCtx = EvaluationContext(
      expressionClassName,
      line,
      expression,
      localVariables.asScala.toSet,
      pckg
    )

    val driver = new Driver:
      protected override def newCompiler(using Context): EvaluationCompiler =
        EvaluationCompiler(using evalCtx)
    val reporter = new StoreReporter

    try
      driver.process(args, reporter)
      val errors = reporter.allErrors
      val error = errors.headOption.map(_.msg.message)
      error.foreach(errorConsumer.accept)
      // reporter.pendingMessages(using null).foreach(d => println(d.msg))
      !reporter.hasErrors
    catch
      case NonFatal(t) =>
        // reporter.pendingMessages(using null).foreach(d => println(d.msg))
        t.printStackTrace()
        errorConsumer.accept(t.getMessage)
        false
