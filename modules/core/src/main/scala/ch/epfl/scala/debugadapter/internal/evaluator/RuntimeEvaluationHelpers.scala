package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._
import scala.meta.trees.*
import scala.meta.Lit
import scala.util.Success
import RuntimeEvaluatorExtractors.*
import scala.meta.Stat
import scala.meta.Term
import scala.util.Failure
import scala.util.Try

private[evaluator] object Helpers {
  def illegalAccess(x: Any, typeName: String) = Fatal {
    new ClassCastException(s"Cannot cast $x to $typeName")
  }

  def fromLitToValue(literal: Lit, classLoader: JdiClassLoader): (Safe[Any], Type) = {
    val tpe = classLoader
      .mirrorOfLiteral(literal.value)
      .map(_.value.`type`)
      .getResult
      .get

    (Safe(literal.value), tpe)
  }

  /* -------------------------------------------------------------------------- */
  /*                                Method lookup                               */
  /* -------------------------------------------------------------------------- */
  private def argsMatch(method: Method, args: Seq[Type], frame: JdiFrame): Boolean =
    method.argumentTypeNames().size() == args.size && areAssignableFrom(method, args, frame)

  /**
   * Look for a method with the given name and arguments types, on the given reference type
   *
   * Encode the method name by default, but can be disabled for methods that are not Scala methods (such as Java <init> methods)
   *
   * If multiple methods are found, a [[Validation.Unrecoverable]] is returned
   *
   * Also, if the method return type is not loaded or prepared, it will be loaded and prepared
   *
   * @param ref the reference type on which to look for the method
   * @param funName the name of the method
   * @param args the arguments types of the method
   * @param frame the current frame
   * @param encode whether to encode the method name or not
   * @return the method, wrapped in a [[Validation]], with its return type loaded and prepared
   */
  def methodsByNameAndArgs(
      ref: ReferenceType,
      funName: String,
      args: Seq[Type],
      frame: JdiFrame,
      encode: Boolean = true
  ): Validation[Method] = {
    val candidates: Seq[Method] = ref
      .methodsByName { if (encode) NameTransformer.encode(funName) else funName }
      .asScalaSeq
      .filter { method => !method.isPrivate && argsMatch(method, args, frame) }
      .toSeq

    val finalCandidates = candidates.size match {
      case 0 | 1 => candidates
      case _ => candidates.filterNot(_.isBridge())
    }

    finalCandidates
      .toValidation(s"Cannot find methods $funName with args types $args on $ref")
      .map(loadClassOnNeed(_, frame))
  }

  /* -------------------------------------------------------------------------- */
  /*                                Type checker                                */
  /* -------------------------------------------------------------------------- */
  def isAssignableFrom(got: Type, expected: Type, frame: JdiFrame): Boolean = {
    def referenceTypesMatch(got: ReferenceType, expected: ReferenceType) = {
      val assignableFrom = expected.classObject().referenceType().methodsByName("isAssignableFrom").get(0)
      val params = Seq(got.classObject()).asJavaList
      expected.classObject
        .invokeMethod(frame.thread, assignableFrom, params, ObjectReference.INVOKE_SINGLE_THREADED)
        .asInstanceOf[BooleanValue]
        .value()
    }

    (got, expected) match {
      case (g: ArrayType, at: ArrayType) => isAssignableFrom(g.componentType, at.componentType, frame)
      case (g: PrimitiveType, pt: PrimitiveType) => got.equals(pt)
      case (g: ReferenceType, ref: ReferenceType) => referenceTypesMatch(g, ref)
      case (_: VoidType, _: VoidType) => true

      case (g: ClassType, pt: PrimitiveType) =>
        isAssignableFrom(g, frame.getPrimitiveBoxedClass(pt), frame)
      case (g: PrimitiveType, ct: ReferenceType) =>
        isAssignableFrom(frame.getPrimitiveBoxedClass(g), ct, frame)

      case _ => false
    }
  }

  def areAssignableFrom(method: Method, args: Seq[Type], frame: JdiFrame): Boolean =
    method.argumentTypes().asScalaSeq.zip(args).forall { case (expected, got) =>
      isAssignableFrom(got, expected, frame)
    }

  /* -------------------------------------------------------------------------- */
  /*                              Looking for $outer                             */
  /* -------------------------------------------------------------------------- */
  def findOuter(tree: RuntimeTree, frame: JdiFrame): Validation[OuterTree] = {
    def outerLookup(ref: ReferenceType) = Validation(ref.fieldByName("$outer")).map(_.`type`()).orElse {
      removeLastInnerTypeFromFQCN(ref.name())
        .map(name => loadClass(name + "$", frame)) match {
        case Some(Safe(Success(Module(mod: ClassType)))) => Valid(mod)
        case _ => Recoverable(s"Cannot find $$outer for $ref")
      }
    }

    for {
      ref <- ifReference(tree)
      outer <- outerLookup(ref)
      outerTree <- OuterTree(tree, outer)
    } yield outerTree
  }

  /* -------------------------------------------------------------------------- */
  /*                               Useful patterns                              */
  /* -------------------------------------------------------------------------- */
  /* Extract reference if there is */
  def ifReference(tree: Validation[RuntimeTree]): Validation[ReferenceType] =
    tree match {
      case Invalid(e) => Recoverable(s"Invalid reference: $e")
      case _ => ifReference(tree.get)
    }

  def ifReference(tree: RuntimeTree): Validation[ReferenceType] =
    tree match {
      case ReferenceTree(ref) => Valid(ref)
      case t => illegalAccess(t, "ReferenceType")
    }

  /* Standardize method calls */
  def extractCall(apply: Stat): Call =
    apply match {
      case apply: Term.Apply => Call(apply.fun, apply.argClause)
      case ColonEndingInfix(apply) => Call(Term.Select(apply.argClause.head, apply.op), List(apply.lhs))
      case apply: Term.ApplyInfix => Call(Term.Select(apply.lhs, apply.op), apply.argClause)
      case apply: Term.ApplyUnary => Call(Term.Select(apply.arg, Term.Name("unary_" + apply.op)), List.empty)
    }

  /* -------------------------------------------------------------------------- */
  /*                           Last nested types regex                          */
  /* -------------------------------------------------------------------------- */
  def getLastInnerType(className: String): Option[String] = {
    val pattern = """(.+\$)([^$]+)$""".r
    className.stripSuffix("$") match {
      case pattern(_, innerType) => Some(innerType)
      case _ => None
    }
  }

  def removeLastInnerTypeFromFQCN(className: String): Option[String] = {
    val pattern = """(.+)\$[\w]+\${0,1}$""".r
    className match {
      case pattern(baseName) => Some(baseName)
      case _ => None
    }
  }

  /* -------------------------------------------------------------------------- */
  /*                  Transformation to static or instance tree                 */
  /* -------------------------------------------------------------------------- */
  def toStaticIfNeeded(field: Field, on: RuntimeTree): FieldTree = on match {
    case cls: ClassTree => StaticFieldTree(field, cls.`type`)
    case eval: RuntimeEvaluationTree => InstanceFieldTree(field, eval)
  }

  def toStaticIfNeeded(
      method: Method,
      args: Seq[RuntimeEvaluationTree],
      on: RuntimeTree
  ): MethodTree = on match {
    case cls: ClassTree => StaticMethodTree(method, args, cls.`type`)
    case eval: RuntimeEvaluationTree => InstanceMethodTree(method, args, eval)
  }

  /* -------------------------------------------------------------------------- */
  /*                                Class helpers                               */
  /* -------------------------------------------------------------------------- */
  def loadClass(name: String, frame: JdiFrame): Safe[JdiClass] =
    frame.classLoader().flatMap(_.loadClass(name))

  def checkClassStatus(tpe: => Type)(name: String, frame: JdiFrame) = Try(tpe) match {
    case Failure(_: ClassNotLoadedException) => loadClass(name, frame).getResult.map(_.cls)
    case Success(value: ClassType) if !value.isPrepared => loadClass(name, frame).getResult.map(_.cls)
    case result => result
  }

  def loadClassOnNeed[T <: TypeComponent](tc: T, frame: JdiFrame): T = {
    checkClassStatus(tc.`type`)(tc.typeName, frame)
    tc
  }

  def searchAllClassesFor(name: String, in: Option[String], frame: JdiFrame): Validation[ClassType] = {
    def fullName = in match {
      case Some(value) if value == name => name // name duplication when implicit apply call
      case Some(value) if value.endsWith("$") => value + name
      case Some(value) => value + "$" + name
      case None => name
    }

    def nameEndMatch(cls: String) =
      (name.endsWith("$"), cls.endsWith("$")) match {
        case (true, true) | (false, false) =>
          cls.split('.').last.split('$').last == name.stripSuffix("$")
        case _ => false
      }

    def candidates =
      frame
        .current()
        .virtualMachine
        .allClasses()
        .asScalaSeq
        .filter { cls => cls.name() == name || nameEndMatch(cls.name()) }

    def finalCandidates =
      candidates.size match {
        case 0 =>
          val topLevelClassName = frame.thisObject
            .map(_.reference.referenceType.name)
            .map(_.split('.').init.mkString(".") + "." + name)
            .getOrElse("")
          loadClass(fullName, frame).getResult
            .orElse { loadClass(topLevelClassName, frame).getResult }
            .map(_.cls)
            .toSeq
        case 1 => candidates
        case _ => candidates.filter(_.name() == fullName)
      }

    finalCandidates
      .toValidation(s"Cannot find module/class $name, has it been loaded ?")
      .map { cls => checkClassStatus(cls)(cls.name(), frame).get.asInstanceOf[ClassType] }
  }
}
