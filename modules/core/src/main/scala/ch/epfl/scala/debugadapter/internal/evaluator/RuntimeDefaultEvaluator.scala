package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger

class RuntimeDefaultEvaluator(val frame: JdiFrame, val logger: Logger) extends RuntimeEvaluator {
  val helper = new RuntimeEvaluationHelpers(frame)
  def evaluate(stat: RuntimeEvaluableTree): Safe[JdiValue] =
    eval(stat).map(_.derefIfRef)

  protected def eval(stat: RuntimeEvaluableTree): Safe[JdiValue] =
    stat match {
      case preEvaluated: PreEvaluatedTree => preEvaluated.value
      case LocalVarTree(varName, _) => Safe.successful(frame.variableByName(varName).map(frame.variableValue).get)
      case primitive: PrimitiveBinaryOpTree => invokePrimitive(primitive)
      case primitive: PrimitiveUnaryOpTree => invokePrimitive(primitive)
      case module: ModuleTree => evaluateModule(module)
      case literal: LiteralTree => evaluateLiteral(literal)
      case ThisTree(obj) => Safe(JdiValue(obj.instances(1).get(0), frame.thread))
      case field: InstanceFieldTree => evaluateField(field)
      case staticField: StaticFieldTree => evaluateStaticField(staticField)
      case instance: NewInstanceTree => instantiate(instance)
      case method: InstanceMethodTree => invoke(method)
      case array: ArrayElemTree => evaluateArrayElement(array)
      case branching: IfTree => evaluateIf(branching)
      case staticMethod: StaticMethodTree => invokeStatic(staticMethod)
      case outer: OuterTree => evaluateOuter(outer)
      case UnitTree => Safe(JdiValue(frame.thread.virtualMachine.mirrorOfVoid, frame.thread))
    }

  /* -------------------------------------------------------------------------- */
  /*                             Literal evaluation                             */
  /* -------------------------------------------------------------------------- */
  def evaluateLiteral(tree: LiteralTree): Safe[JdiValue] =
    for {
      loader <- frame.classLoader()
      value <- tree.value
      result <- loader.mirrorOfLiteral(value)
    } yield result

  /* -------------------------------------------------------------------------- */
  /*                              Outer evaluation                              */
  /* -------------------------------------------------------------------------- */
  def evaluateOuter(tree: OuterTree): Safe[JdiValue] =
    tree match {
      case OuterModuleTree(module) => evaluateModule(module)
      case outerClass: OuterClassTree =>
        eval(outerClass.inner).map(_.asObject.getField("$outer"))
    }

  /* -------------------------------------------------------------------------- */
  /*                              Field evaluation                              */
  /* -------------------------------------------------------------------------- */
  def evaluateField(tree: InstanceFieldTree): Safe[JdiValue] =
    eval(tree.qual).map { value =>
      JdiValue(value.asObject.reference.getValue(tree.field), frame.thread)
    }

  def evaluateStaticField(tree: StaticFieldTree): Safe[JdiValue] =
    Safe { JdiValue(tree.on.getValue(tree.field), frame.thread) }

  /* -------------------------------------------------------------------------- */
  /*                              Method evaluation                             */
  /* -------------------------------------------------------------------------- */
  def invokeStatic(tree: StaticMethodTree): Safe[JdiValue] =
    for {
      args <- tree.args.map(eval).traverse
      loader <- frame.classLoader()
      argsBoxedIfNeeded <- loader.boxUnboxOnNeed(tree.method.argumentTypes(), args)
      result <- JdiClass(tree.on, frame.thread).invokeStatic(tree.method, argsBoxedIfNeeded)
    } yield result

  def invokePrimitive(tree: PrimitiveBinaryOpTree): Safe[JdiValue] =
    for {
      lhs <- eval(tree.lhs).flatMap(_.unboxIfPrimitive)
      rhs <- eval(tree.rhs).flatMap(_.unboxIfPrimitive)
      loader <- frame.classLoader()
      result <- tree.op.evaluate(lhs, rhs, loader)
    } yield result

  def invokePrimitive(tree: PrimitiveUnaryOpTree): Safe[JdiValue] =
    for {
      rhs <- eval(tree.rhs).flatMap(_.unboxIfPrimitive)
      loader <- frame.classLoader()
      result <- tree.op.evaluate(rhs, loader)
    } yield result

  def invoke(tree: InstanceMethodTree): Safe[JdiValue] =
    for {
      qualValue <- eval(tree.qual)
      argsValues <- tree.args.map(eval).traverse
      loader <- frame.classLoader()
      argsBoxedIfNeeded <- loader.boxUnboxOnNeed(tree.method.argumentTypes(), argsValues)
      result <- qualValue.asObject.invoke(tree.method, argsBoxedIfNeeded)
    } yield result

  /* -------------------------------------------------------------------------- */
  /*                              Module evaluation                             */
  /* -------------------------------------------------------------------------- */
  def evaluateModule(tree: ModuleTree): Safe[JdiValue] =
    tree match {
      case TopLevelModuleTree(mod) => Safe(JdiObject(mod.instances(1).get(0), frame.thread))
      case NestedModuleTree(mod, of) => helper.initializeModule(mod, eval(of))
      // TODO: change the $of attribute to be a Method validated by the validator to avoid crashes at evaluation time
    }

  /* -------------------------------------------------------------------------- */
  /*                                Instantiation                               */
  /* -------------------------------------------------------------------------- */
  def instantiate(tree: NewInstanceTree): Safe[JdiObject] =
    for {
      args <- tree.init.args.map(eval).traverse
      loader <- frame.classLoader()
      boxedUnboxedArgs <- loader.boxUnboxOnNeed(tree.init.method.argumentTypes(), args)
      instance <- JdiClass(tree.`type`, frame.thread).newInstance(tree.init.method, boxedUnboxedArgs)
    } yield instance

  /* -------------------------------------------------------------------------- */
  /*                          Array accessor evaluation                         */
  /* -------------------------------------------------------------------------- */
  def evaluateArrayElement(tree: ArrayElemTree): Safe[JdiValue] =
    for {
      array <- eval(tree.array)
      index <- eval(tree.index).flatMap(_.unboxIfPrimitive).flatMap(_.toInt)
    } yield array.asArray.getValue(index)

  /* -------------------------------------------------------------------------- */
  /*                             If tree evaluation                             */
  /* -------------------------------------------------------------------------- */
  def evaluateIf(tree: IfTree): Safe[JdiValue] =
    for {
      predicate <- eval(tree.p).flatMap(_.unboxIfPrimitive).flatMap(_.toBoolean)
      value <- if (predicate) eval(tree.thenp) else eval(tree.elsep)
    } yield value
}

object RuntimeDefaultEvaluator {
  def apply(frame: JdiFrame, logger: Logger): RuntimeDefaultEvaluator =
    new RuntimeDefaultEvaluator(frame, logger)
}
