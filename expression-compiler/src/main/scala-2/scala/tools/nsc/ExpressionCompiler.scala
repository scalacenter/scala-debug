package scala.tools.nsc

import java.nio.file.Path
import java.util.function.Consumer
import java.{util => ju}
import scala.collection.JavaConverters._
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.concurrent.duration.Duration

final class ExpressionCompiler {
  def compile(
      expressionDir: Path,
      expressionClassName: String,
      valuesByNameIdentName: String,
      classPath: String,
      code: String,
      line: Int,
      expression: String,
      defNames: ju.Set[String],
      errorConsumer: Consumer[String],
      timeoutMillis: Long
  ): Boolean = {
    val settings = new Settings
    settings.classpath.value = classPath
    settings.outputDirs.setSingleOutput(expressionDir.toString)
    val reporter = new StoreReporter
    val global = new EvalGlobal(
      settings,
      reporter,
      line,
      expression,
      defNames.asScala.toSet,
      expressionClassName,
      valuesByNameIdentName
    )
    val source = new BatchSourceFile("<source>", code)

    try {
      global
        .askForResponse { () =>
          val compilerRun = new global.Run()
          compilerRun.compileSources(List(source))
        }
        .get(timeoutMillis)
        .getOrElse(
          throw new ju.concurrent.TimeoutException(
            s"Compilation timed out after $timeoutMillis ms"
          )
        )
    } finally {
      global.askShutdown()
      global.close()
    }

    val error = reporter.infos.find(_.severity == reporter.ERROR).map(_.msg)
    error.foreach(errorConsumer.accept)
    error.isEmpty
  }
}
