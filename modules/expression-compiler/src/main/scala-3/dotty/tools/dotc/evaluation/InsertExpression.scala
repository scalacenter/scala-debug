package dotty.tools.dotc.evaluation

import dotty.tools.dotc.ExpressionContext
import dotty.tools.dotc.ast.untpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.util.SourceFile
import java.nio.charset.StandardCharsets
import dotty.tools.io.VirtualFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.NoSourcePosition
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SrcPos

/**
 * This phase:
 * - inserts the expression that is being evaluated in the line of the breakpoint
 * - inserts `Expression` class in a proper package
 */
class InsertExpression(using exprCtx: ExpressionContext) extends Phase:
  private var expressionInserted = false

  override def phaseName: String = InsertExpression.name
  override def isCheckable: Boolean = false

  private val evaluationClassSource =
    s"""|class ${exprCtx.expressionClassName}(names: Array[String], values: Array[Any]):
        |  import java.lang.reflect.InvocationTargetException
        |  val classLoader = getClass.getClassLoader.nn
        |
        |  def evaluate(): Any =
        |    ()
        |
        |  def getLocalValue(name: String): Any =
        |    val idx = names.indexOf(name)
        |    if idx == -1 then throw new NoSuchElementException(name)
        |    else values(idx)
        |
        |  def setLocalValue(name: String, value: Any): Any =
        |    val idx = names.indexOf(name)
        |    if idx == -1 then throw new NoSuchElementException(name)
        |    else values(idx) = value
        |
        |  def callMethod(obj: Any, className: String, methodName: String, paramTypesNames: Array[String], returnTypeName: String, args: Array[Object]): Any =
        |    val clazz = classLoader.loadClass(className).nn
        |    val method = clazz.getDeclaredMethods.nn
        |      .map(_.nn)
        |      .find { m =>
        |        m.getName == methodName &&
        |          m.getReturnType.nn.getName == returnTypeName &&
        |          m.getParameterTypes.nn.map(_.nn.getName).toSeq == paramTypesNames.toSeq
        |      }
        |      .getOrElse(throw new NoSuchMethodException(methodName))
        |    method.setAccessible(true)
        |    unwrapException(method.invoke(obj, args*))
        |
        |  def callConstructor(className: String, paramTypesNames: Array[String], args: Array[Object]): Any =
        |    val clazz = classLoader.loadClass(className).nn
        |    val constructor = clazz.getConstructors.nn
        |      .map(_.nn)
        |      .find { c => c.getParameterTypes.nn.map(_.nn.getName).toSeq == paramTypesNames.toSeq }
        |      .getOrElse(throw new NoSuchMethodException(s"new $$className"))
        |    constructor.setAccessible(true)
        |    unwrapException(constructor.newInstance(args*))
        |
        |  def getField(obj: Any, className: String, fieldName: String): Any =
        |    val clazz = classLoader.loadClass(className).nn
        |    val field = clazz.getDeclaredField(fieldName).nn
        |    field.setAccessible(true)
        |    field.get(obj)
        |
        |  def setField(obj: Any, className: String, fieldName: String, value: Any): Unit =
        |    val clazz = classLoader.loadClass(className).nn
        |    val field = clazz.getDeclaredField(fieldName).nn
        |    field.setAccessible(true)
        |    field.set(obj, value)
        |
        |  def getOuter(obj: Any, outerTypeName: String): Any =
        |    val clazz = obj.getClass
        |    val field = getSuperclassIterator(clazz)
        |      .flatMap(_.getDeclaredFields.nn.toSeq)
        |      .map(_.nn)
        |      .find { field => field.getName == "$$outer" && field.getType.nn.getName == outerTypeName }
        |      .getOrElse(throw new NoSuchFieldException("$$outer"))
        |    field.setAccessible(true)
        |    field.get(obj)
        |
        |  def getStaticObject(className: String): Any =
        |    val clazz = classLoader.loadClass(className).nn
        |    val field = clazz.getDeclaredField("MODULE$$").nn
        |    field.setAccessible(true)
        |    field.get(null)
        |
        |  def getSuperclassIterator(clazz: Class[?] | Null): Iterator[Class[?]] =
        |    Iterator.iterate(clazz)(_.nn.getSuperclass).takeWhile(_ != null).map(_.nn)
        |
        |  // a fake method that is used between the extract-expression and the resolve-reflect-eval phases, 
        |  // which transforms them to calls of one of the methods defined above.
        |  def reflectEval(qualifier: Object, term: String, args: Array[Object]): Any = ???
        |
        |  private def unwrapException(f: => Any): Any =
        |    try f
        |    catch
        |      case e: InvocationTargetException => throw e.getCause.nn
        |
        |  extension [T] (x: T | Null)
        |    private def nn: T = x.asInstanceOf[T]
        |""".stripMargin

  override def run(using Context): Unit =
    val inserter = Inserter(parseExpression, parseEvaluationClass)
    ctx.compilationUnit.untpdTree = inserter.transform(ctx.compilationUnit.untpdTree)

  class Inserter(expression: Tree, expressionClass: Seq[Tree]) extends UntypedTreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case tree: PackageDef =>
          val transformed = super.transform(tree).asInstanceOf[PackageDef]
          if expressionInserted then
            // set to `false` to prevent inserting `Expression` class in other `PackageDef`s
            expressionInserted = false
            cpy.PackageDef(transformed)(
              transformed.pid,
              transformed.stats ++ expressionClass.map(
                _.withSpan(tree.span)
              )
            )
          else transformed
        case tree @ DefDef(name, paramss, tpt, rhs) if rhs != EmptyTree && isOnBreakpoint(tree) =>
          cpy.DefDef(tree)(
            name,
            paramss,
            tpt,
            mkExprBlock(expression, tree.rhs)
          )
        case tree @ Match(selector, caseDefs) if isOnBreakpoint(tree) || caseDefs.exists(isOnBreakpoint) =>
          // the expression is on the match or a case of the match
          // if it is on the case of the match the program could pause on the pattern, the guard or the body
          // we assume it pauses on the pattern because that is the first instruction
          // in that case we cannot compile the expression val in the pattern, but we can compile it in the selector
          cpy.Match(tree)(mkExprBlock(expression, selector), caseDefs)
        case tree @ ValDef(name, tpt, rhs) if isOnBreakpoint(tree) =>
          cpy.ValDef(tree)(name, tpt, mkExprBlock(expression, tree.rhs))
        case tree: (Ident | Select | GenericApply | Literal | This | New | InterpolatedString | OpTree | Tuple |
              Assign | Block) if isOnBreakpoint(tree) =>
          mkExprBlock(expression, tree)

        // for loop: we insert the expression on the first enumeration
        case tree @ ForYield(enums, rhs) if isOnBreakpoint(tree) =>
          ForYield(transform(enums.head) :: enums.tail, rhs)
        case tree @ ForDo(enums, rhs) if isOnBreakpoint(tree) =>
          ForDo(transform(enums.head) :: enums.tail, rhs)

        // generator of for loop: we insert the expression on the rhs
        case tree @ GenFrom(pat, rhs, checkMode) if isOnBreakpoint(tree) =>
          GenFrom(pat, mkExprBlock(expression, rhs), checkMode)
        case tree @ GenAlias(pat, rhs) if isOnBreakpoint(tree) =>
          GenAlias(pat, mkExprBlock(expression, rhs))

        case tree => super.transform(tree)

  private def parseExpression(using Context): Tree =
    val prefix =
      s"""|object Expression:
          |  {
          |    """.stripMargin
    // don't use stripMargin on wrappedExpression because expression can contain a line starting with `  |`
    val wrappedExpression = prefix + exprCtx.expression + "\n  }\n"
    val expressionFile = SourceFile.virtual("<expression>", exprCtx.expression)
    val contentBytes = wrappedExpression.getBytes(StandardCharsets.UTF_8)
    val wrappedExpressionFile =
      new VirtualFile("<wrapped-expression>", contentBytes)
    val sourceFile =
      new SourceFile(wrappedExpressionFile, wrappedExpression.toArray):
        override def start: Int =
          // prefix.size depends on the OS
          -prefix.size
        override def underlying: SourceFile = expressionFile
        override def atSpan(span: Span): SourcePosition =
          if (span.exists) SourcePosition(this, span)
          else NoSourcePosition

    parse(sourceFile)
      .asInstanceOf[PackageDef]
      .stats
      .head
      .asInstanceOf[ModuleDef]
      .impl
      .body
      .head

  private def parseEvaluationClass(using Context): Seq[Tree] =
    val sourceFile =
      SourceFile.virtual("<evaluation class>", evaluationClassSource)
    parse(sourceFile).asInstanceOf[PackageDef].stats

  private def parse(sourceFile: SourceFile)(using Context): Tree =
    val newCtx = ctx.fresh.setSource(sourceFile)
    val parser = Parsers.Parser(sourceFile)(using newCtx)
    parser.parse()

  private def isOnBreakpoint(tree: Tree)(using Context): Boolean =
    val startLine =
      if tree.span.exists then tree.sourcePos.startLine + 1 else -1
    startLine == exprCtx.breakpointLine

  private def mkExprBlock(expr: Tree, tree: Tree)(using
      Context
  ): Tree =
    if expressionInserted then
      warnOrError("expression already inserted", tree.srcPos)
      tree
    else
      expressionInserted = true
      val valDef = ValDef(exprCtx.expressionTermName, TypeTree(), expr)
      Block(List(valDef), tree)

  // only fails in test mode
  private def warnOrError(msg: String, srcPos: SrcPos)(using Context): Unit =
    if exprCtx.testMode then report.error(msg, srcPos)
    else report.warning(msg, srcPos)

object InsertExpression:
  val name: String = "insert-expression"
