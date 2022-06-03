package dotty.tools.dotc.evaluation

import dotty.tools.dotc.EvaluationContext
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.SymUtils.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.transform.MacroTransform
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.typer.Inliner
import dotty.tools.dotc.report

class ExtractExpression(using evalCtx: EvaluationContext)
    extends MacroTransform
    with DenotTransformer:
  override def phaseName: String = ExtractExpression.name

  /**
   * Change the return type of the `evaluate` method
   * and update the owner and types of the symDenotations inserted into `evaluate`.
   */
  override def transform(ref: SingleDenotation)(using
      Context
  ): SingleDenotation =
    ref match
      case ref: SymDenotation if isExpressionVal(ref.symbol.maybeOwner) =>
        // update owner of the symDenotation, e.g. local vals
        // that are extracted out of the expression val to the evaluate method
        ref.copySymDenotation(owner = evalCtx.evaluateMethod)
      case _ =>
        ref

  override def transformPhase(using Context): Phase = this.next

  override protected def newTransformer(using Context): Transformer =
    new Transformer:
      var expressionTree: Tree = _
      override def transform(tree: Tree)(using Context): Tree =
        tree match
          case PackageDef(pid, stats) =>
            val evaluationClassDef =
              stats.find(_.symbol == evalCtx.evaluationClass)
            val others = stats.filter(_.symbol != evalCtx.evaluationClass)
            val transformedStats = (others ++ evaluationClassDef).map(transform)
            PackageDef(pid, transformedStats)
          case tree: ValDef if isExpressionVal(tree.symbol) =>
            expressionTree = tree.rhs
            evalCtx.store(tree.symbol)
            unitLiteral
          case tree: DefDef if tree.symbol == evalCtx.evaluateMethod =>
            val transformedExpr =
              ExpressionTransformer.transform(expressionTree)
            cpy.DefDef(tree)(rhs = transformedExpr)
          case tree =>
            super.transform(tree)

  private object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        // static object
        case tree: Ident if isStaticObject(tree.symbol) =>
          getStaticObject(tree.symbol.moduleClass)
        case tree: Select if isStaticObject(tree.symbol) =>
          getStaticObject(tree.symbol.moduleClass)

        // non-static object
        case tree: Ident if isNonStaticObject(tree.symbol) =>
          callMethod(getThis, tree.symbol.asTerm, List.empty, tree.tpe)
        case tree: Select if isNonStaticObject(tree.symbol) =>
          val qualifier = transform(tree.qualifier)
          callMethod(qualifier, tree.symbol.asTerm, List.empty, tree.tpe)

        // local variable
        case tree: Ident if isLocalVariable(tree.symbol) =>
          if tree.symbol.is(Lazy) then
            report.error(
              s"Evaluation of local lazy val not supported: ${tree.symbol}"
            )
            tree
          else
            // a local variable can be captured by a class or method
            val owner = tree.symbol.owner
            val candidates = evalCtx.expressionSymbol.ownersIterator
              .takeWhile(_ != owner)
              .filter(s => s.isClass || s.is(Method))
              .toSeq
            val capturer = candidates
              .findLast(_.isClass)
              .orElse(candidates.find(_.is(Method)))
            capturer match
              case Some(cls) if cls.isClass =>
                getClassCapture(tree.symbol, cls, tree.tpe)
              case Some(method) =>
                getMethodCapture(tree.symbol, method, tree.tpe)
              case None => getLocalValue(tree.symbol, tree.tpe)

        // assignement to local variable
        case tree @ Assign(lhs, rhs) if isLocalVariable(lhs.symbol) =>
          report.error(
            s"Assignment to local variable not supported: ${lhs.symbol}"
          )
          Assign(lhs, transform(rhs))

        // inaccessible fields
        case tree: (Ident | Select) if isInaccessibleField(tree.symbol) =>
          val qualifier = getTransformedQualifier(tree)
          getField(qualifier, tree.symbol.asTerm, tree.tpe)

        // assignment to inaccessible fields
        case tree @ Assign(lhs, rhs) if isInaccessibleField(lhs.symbol) =>
          val qualifier = lhs match
            case lhs: Ident =>
              if isStaticObject(lhs.symbol.owner)
              then getStaticObject(lhs.symbol.owner)
              else getThis
            case lhs: Select => transform(lhs.qualifier)
          setField(qualifier, lhs.symbol.asTerm, transform(rhs), tree.tpe)

        // this or outer this
        case tree @ This(Ident(name)) =>
          thisOrOuterValue(tree.symbol)

        // inaccessible constructors
        case tree @ Apply(Select(New(cls), _), _)
            if isInaccessibleConstructor(tree.symbol) =>
          val args = tree.args.map(transform)
          val qualifier = getTransformedQualifier(cls)
          callConstructor(qualifier, tree.symbol.asTerm, args, tree.tpe)
        case tree: TypeApply if isInaccessibleConstructor(tree.symbol) =>
          transform(tree.fun)
        case tree: (Select | Ident) if isInaccessibleConstructor(tree.symbol) =>
          val qualifier = getTransformedQualifier(tree)
          callConstructor(qualifier, tree.symbol.asTerm, List.empty, tree.tpe)

        // inaccessible methods
        case tree: Apply if isInaccessibleMethod(tree.symbol) =>
          val args = tree.args.map(transform)
          val qualifier = getTransformedQualifier(tree.fun)
          callMethod(qualifier, tree.symbol.asTerm, args, tree.tpe)
        case tree: TypeApply if isInaccessibleMethod(tree.symbol) =>
          transform(tree.fun)
        case tree: (Select | Ident) if isInaccessibleMethod(tree.symbol) =>
          val qualifier = getTransformedQualifier(tree)
          callMethod(qualifier, tree.symbol.asTerm, List.empty, tree.tpe)

        case tree => super.transform(tree)

    private def getTransformedQualifier(tree: Tree)(using Context): Tree =
      tree match
        case Ident(_) =>
          val owner = tree.symbol.owner
          if isStaticObject(owner)
          then getStaticObject(owner)
          else thisOrOuterValue(owner)
        case Select(qualifier, _) => transform(qualifier)
        case Apply(fun, _) => getTransformedQualifier(fun)
        case TypeApply(fun, _) => getTransformedQualifier(fun)

  end ExpressionTransformer

  private def isExpressionVal(sym: Symbol)(using Context): Boolean =
    sym.name == evalCtx.expressionTermName

  // symbol can be a class or a method
  private def thisOrOuterValue(symbol: Symbol)(using Context): Tree =
    val cls = symbol.ownersIterator.find(_.isClass).get
    val owners = evalCtx.classOwners.toSeq
    val target = owners.indexOf(cls)
    owners
      .take(target + 1)
      .drop(1)
      .foldLeft(getThis) { (innerObj, outerSym) =>
        getOuter(innerObj, outerSym.thisType)
      }

  private def getThis(using Context): Tree =
    reflectEval(
      None,
      EvaluationStrategy.This,
      List.empty,
      Some(evalCtx.classOwners.head.thisType)
    )

  private def getOuter(qualifier: Tree, tpe: Type)(using
      Context
  ): Tree =
    reflectEval(
      Some(qualifier),
      EvaluationStrategy.Outer,
      List.empty,
      Some(tpe)
    )

  private def getLocalValue(variable: Symbol, tpe: Type)(using Context): Tree =
    reflectEval(
      None,
      EvaluationStrategy.LocalValue(variable.asTerm),
      List.empty,
      Some(tpe)
    )

  private def getClassCapture(variable: Symbol, cls: Symbol, tpe: Type)(using
      Context
  ): Tree =
    reflectEval(
      Some(thisOrOuterValue(cls)),
      EvaluationStrategy.ClassCapture(variable.asTerm, cls.asClass),
      List.empty,
      Some(tpe)
    )

  private def getMethodCapture(variable: Symbol, method: Symbol, tpe: Type)(
      using Context
  ): Tree =
    reflectEval(
      None,
      EvaluationStrategy.MethodCapture(variable.asTerm, method.asTerm),
      List.empty,
      Some(tpe)
    )

  private def getStaticObject(obj: Symbol)(using ctx: Context): Tree =
    reflectEval(
      None,
      EvaluationStrategy.StaticObject(obj.asClass),
      List.empty,
      None
    )

  private def getField(qualifier: Tree, field: TermSymbol, tpe: Type)(using
      Context
  ): Tree =
    reflectEval(
      Some(qualifier),
      EvaluationStrategy.Field(field),
      List.empty,
      Some(tpe)
    )

  private def setField(
      qualifier: Tree,
      field: TermSymbol,
      rhs: Tree,
      tpe: Type
  )(using
      Context
  ): Tree =
    reflectEval(
      Some(qualifier),
      EvaluationStrategy.FieldAssign(field),
      List(rhs),
      Some(tpe)
    )

  private def callMethod(
      qualifier: Tree,
      fun: TermSymbol,
      args: List[Tree],
      tpe: Type
  )(using Context): Tree =
    reflectEval(
      Some(qualifier),
      EvaluationStrategy.MethodCall(fun),
      args,
      Some(tpe)
    )

  private def callConstructor(
      qualifier: Tree,
      ctr: TermSymbol,
      args: List[Tree],
      tpe: Type
  )(using Context): Tree =
    reflectEval(
      Some(qualifier),
      EvaluationStrategy.ConstructorCall(ctr, ctr.owner.asClass),
      args,
      Some(tpe)
    )

  private def reflectEval(
      qualifier: Option[Tree],
      strategy: EvaluationStrategy,
      args: List[Tree],
      tpe: Option[Type]
  )(using
      Context
  ): Tree =
    val tree =
      Apply(
        Select(This(evalCtx.evaluationClass), termName("reflectEval")),
        List(
          qualifier.getOrElse(nullLiteral),
          Literal(Constant(strategy.toString)),
          JavaSeqLiteral(args, TypeTree(ctx.definitions.ObjectType))
        )
      )
    tree.putAttachment(EvaluationStrategy, strategy)
    tpe.map(cast(tree, _)).getOrElse(tree)

  private def cast(tree: Tree, tpe: Type)(using Context): Tree =
    val widenDealiasTpe = tpe.widenDealias
    if isTypeAccessible(widenDealiasTpe.typeSymbol.asType)
    then tree.cast(widenDealiasTpe)
    else tree

  private def isStaticObject(symbol: Symbol)(using Context): Boolean =
    symbol.is(Module) && symbol.isStatic && !symbol.isRoot

  private def isNonStaticObject(symbol: Symbol)(using Context): Boolean =
    symbol.is(Module) && !symbol.isStatic && !symbol.isRoot

  /**
   * The symbol is a field and the expression class cannot access it
   * either because it is private or it belongs to an inacessible type
   */
  private def isInaccessibleField(symbol: Symbol)(using Context): Boolean =
    symbol.isField && symbol.owner.isType && !isTermAccessible(
      symbol.asTerm,
      symbol.owner.asType
    )

  /**
   * The symbol is a real method and the expression class cannot access it
   * either because it is private or it belongs to an inaccessible type
   */
  private def isInaccessibleMethod(symbol: Symbol)(using Context): Boolean =
    symbol.isRealMethod && (
      !symbol.owner.isType ||
        !isTermAccessible(symbol.asTerm, symbol.owner.asType)
    )

  /**
   * The symbol is a constructor and the expression class cannot access it
   * either because it is an inaccessible method or it belong to a nested type (not static)
   */
  private def isInaccessibleConstructor(symbol: Symbol)(using
      Context
  ): Boolean =
    symbol.isConstructor &&
      (isInaccessibleMethod(symbol) || !symbol.owner.isStatic)

  private def isLocalVariable(symbol: Symbol)(using Context): Boolean =
    !symbol.is(Method) &&
      symbol.isLocalToBlock &&
      symbol.ownersIterator.forall(_ != evalCtx.evaluateMethod)

  // Check if a term is accessible from the expression class
  private def isTermAccessible(symbol: TermSymbol, owner: TypeSymbol)(using
      Context
  ): Boolean =
    !symbol.isPrivate && isTypeAccessible(owner)

    // Check if a type is accessible from the expression class
  private def isTypeAccessible(symbol: TypeSymbol)(using Context): Boolean =
    !symbol.isLocal && symbol.ownersIterator.forall(sym =>
      sym.isPublic || sym.privateWithin.is(PackageClass)
    )

object ExtractExpression:
  val name: String = "extract-expression"
