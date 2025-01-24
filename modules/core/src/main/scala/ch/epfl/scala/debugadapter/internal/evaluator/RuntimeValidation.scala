package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.NameTransformer
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import ch.epfl.scala.debugadapter.internal.evaluator.RuntimeEvaluationTree.*
import ch.epfl.scala.debugadapter.internal.evaluator.RuntimePrimitiveOps.*
import com.sun.jdi

import scala.jdk.CollectionConverters.*
import scala.meta.parsers.*
import scala.meta.trees.*
import scala.meta.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

private[evaluator] class RuntimeValidation(frame: JdiFrame, sourceLookUp: SourceLookUpProvider, preEvaluation: Boolean)(
    implicit logger: Logger
) {
  private val evaluation = new RuntimeEvaluation(frame, logger)
  private def classLoader: Validation[JdiClassLoader] =
    Validation.fromTry(frame.classLoader().getResult)

  private lazy val thisTree: Validation[RuntimeEvaluationTree] =
    Validation.fromOption(
      frame.thisObject.map(ths => This(ths.reference.referenceType())),
      "`this` is not available in a static context"
    )

  private lazy val declaringType: Validation[jdi.ReferenceType] =
    Validation(frame.current().location().declaringType())

  private lazy val currentPackage: Validation[String] =
    declaringType.map(_.name.reverse.dropWhile(_ != '.').reverse)

  def validate(expression: String): Validation[RuntimeEvaluationTree] =
    parse(expression).flatMap(validateAsValue)

  private def parse(expression: String): Validation[Stat] =
    expression.parse[Stat] match {
      case err: Parsed.Error => Fatal(err.details)
      case Parsed.Success(tree) => Valid(tree)
    }

  private def validateAsValue(expression: Stat): Validation[RuntimeEvaluationTree] =
    expression match {
      case lit: Lit => validateLiteral(lit)
      case Term.Name(name) =>
        findVariable(name)
          .orElse(thisTree.flatMap(findMemberOrModule(name, _)))
          .orElse(declaringType.flatMap(findStaticMember(name, _)))
          .orElse(findTopLevelModule(name))
      case _: Term.This => thisTree
      case sup: Term.Super => Recoverable("Super not (yet) supported at runtime")
      case expression @ Term.Apply.After_4_6_0(Term.Name(fun), _) =>
        for {
          call <- standardize(expression)
          method <-
            validateMethod(call)
              .orElse(findClass("Predef").flatMap(x => findStaticMethodBySignedName(x.`type`, fun, call.args)))
        } yield method
      case _: Term.Apply | _: Term.ApplyInfix | _: Term.ApplyUnary => standardize(expression).flatMap(validateMethod)
      case select: Term.Select =>
        validateAsValueOrClass(select.qual)
          .flatMap {
            case Left(qualifier) => findMemberOrModule(select.name.value, qualifier)
            case Right(qualifier) => findStaticMember(select.name.value, qualifier)
          }
          .orElse(findTopLevelModule(select.qual.toString, select.name.value))
      case branch: Term.If => validateIf(branch)
      case instance: Term.New => validateNew(instance)
      case block: Term.Block => validateBlock(block)
      case assign: Term.Assign => validateAssign(assign)
      case _ => Recoverable(s"Cannot evaluate '$expression' at runtime")
    }

  private def validateAsValueOrClass(expression: Stat): Validation[Either[RuntimeEvaluationTree, jdi.ReferenceType]] =
    validateAsValue(expression).map(Left.apply).orElse(validateAsClass(expression).map(Right.apply))

  private def validateAsClass(expression: Stat): Validation[jdi.ReferenceType] =
    expression match {
      case Term.Name(name) => findClass(name).map(_.`type`)
      case Term.Select(qualifier, Name(name)) =>
        validateAsValueOrClass(qualifier)
          .flatMap {
            case Left(qualifier) => findMemberClass(name, qualifier).map(_.`type`)
            case Right(qualifier) => findStaticClass(name, qualifier).map(_.`type`)
          }
          .orElse(findQualifiedClass(name, qualifier.toString))
      case _ => Recoverable("not a class")
    }

  private def preEvaluate(tree: RuntimeEvaluationTree): RuntimeEvaluationTree = {
    def eval = {
      val value = evaluation.evaluate(tree)
      var tpe = value.extract(_.value.`type`)
      Validation.fromTry(tpe).map(Value(value, _)).getOrElse(tree)
    }

    if (preEvaluation) {
      tree match {
        case CallBinaryOp(_: Value, _: Value, _) => eval
        case ArrayElem(_: Value, _: Value, _) => eval
        case If(Value(cond, _), thenp, elsep, _) =>
          val boolean = for {
            cond <- cond
            unboxed <- cond.unboxIfPrimitive
            bool <- unboxed.toBoolean
          } yield bool
          boolean.extract match {
            case Success(true) => thenp
            case Success(false) => elsep
            case _ => tree
          }
        case InstanceField(_, _: Value) => eval
        case _: StaticField => eval
        case LocalVar(_, _) => eval
        case CallUnaryOp(_: Value, _) => eval
        case NestedModule(_, CallInstanceMethod(_, _, _: Value)) => eval
        case _: StaticModule => eval
        case _: This => eval
        case _: Literal => eval
        case _ => tree
      }
    } else tree
  }

  private def validateBlock(block: Term.Block): Validation[RuntimeEvaluationTree] =
    block.stats.foldLeft(unitTree) {
      case (Valid(_), stat) => validateAsValue(stat)
      case (err: Invalid, _) => err
    }

  private val unitTree: Validation[RuntimeEvaluationTree] = validateLiteral(Lit.Unit())

  private def validateLiteral(lit: Lit): Validation[RuntimeEvaluationTree] =
    classLoader.map { loader =>
      val tpe =
        if (lit.value == null) null
        else loader.mirrorOfLiteral(lit.value).map(_.value.`type`).getResult.get
      preEvaluate(Literal(lit.value, tpe))
    }

  private def findField(
      qualifier: RuntimeEvaluationTree,
      name: String,
      preevaluate: Boolean = preEvaluation
  ): Validation[RuntimeEvaluationTree] =
    for {
      qualifierTpe <- asReference(qualifier.`type`)
      field <- findField(name, qualifierTpe)
      fieldTree <- asInstanceField(field, qualifier, preevaluate)
    } yield fieldTree

  private def findStaticField(
      qualifier: jdi.ReferenceType,
      name: String,
      preevaluate: Boolean = preEvaluation
  ): Validation[RuntimeEvaluationTree] =
    findField(name, qualifier).flatMap(asStaticField(_, preevaluate))

  private def findVariable(name: String, preevaluate: Boolean = preEvaluation): Validation[RuntimeEvaluationTree] = {
    val encodedName = NameTransformer.encode(name)
    Validation
      .fromOption(frame.variableByName(encodedName), s"$name is not a local variable")
      .filter("value could be a by-name argument")(_.`type`.name != "scala.Function0")
      .map { localVar =>
        val v = LocalVar(encodedName, localVar.`type`)
        if (preevaluate) preEvaluate(v) else v
      }
  }

  private def findField(name: String, ref: jdi.ReferenceType): Validation[jdi.Field] = {
    val encodedName = NameTransformer.encode(name)
    def fieldOpt = Option(ref.fieldByName(encodedName))
      .orElse(ref.visibleFields.asScala.find(_.name.endsWith("$" + encodedName)))
    Validation.fromOption(fieldOpt, s"$name is not a field in ${ref.name}").tap(loadClassOnNeed)
  }

  private sealed trait RuntimeClass {
    def `type`: jdi.ClassType
  }
  private case class MemberClass(`type`: jdi.ClassType, qualifier: RuntimeEvaluationTree) extends RuntimeClass
  private case class StaticOrTopLevelClass(`type`: jdi.ClassType) extends RuntimeClass

  private def findClass(name: String): Validation[RuntimeClass] =
    thisTree
      .flatMap(findMemberClass(name, _))
      .orElse(declaringType.flatMap(findStaticClass(name, _)))
      .orElse(findTopLevelClass(name), resetError = true)

  private def findStaticClass(name: String, qualifier: jdi.ReferenceType): Validation[RuntimeClass] =
    findInnerClass(name, qualifier)
      .filter("class is not a static class")(_.isStatic)
      .map(StaticOrTopLevelClass.apply)

  private def findMemberClass(name: String, qualifier: RuntimeEvaluationTree): Validation[RuntimeClass] =
    findInnerClass(name, qualifier.`type`)
      .map(MemberClass(_, qualifier))
      .orElse(findOuter(qualifier).flatMap(outer => findMemberClass(name, outer)))

  private def findInnerClass(name: String, tpe: jdi.Type): Validation[jdi.ClassType] = {
    val classes = getAllFullyQualifiedClassNames(name)
    linearize(tpe).iterator
      .map { tpe =>
        val encodedQualifier = tpe.name.split('.').map(NameTransformer.encode).mkString(".")
        classes.filter(_.contains(encodedQualifier))
      }
      .find(_.nonEmpty)
      .toSeq
      .flatten
      .validateSingle(s"Cannot find class $name in linearization of ${tpe.name}")
      .flatMap(loadClass)
  }

  private def linearize(tpe: jdi.Type): Seq[jdi.ReferenceType] = {
    tpe match {
      case cls: jdi.ClassType =>
        cls +: (linearize(cls.superclass()) ++ cls.allInterfaces().asScala.reverseIterator).distinct
      case _ => Seq.empty
    }
  }

  private def findQualifiedClass(name: String, packageName: String): Validation[jdi.ClassType] = {
    val classes = getAllFullyQualifiedClassNames(name)
    val encodedPackage = packageName.split('.').map(NameTransformer.encode).mkString(".")
    classes
      .filter(_.contains(encodedPackage))
      .validateSingle(s"Cannot find single class $name in package $packageName")
      .flatMap(loadClass)
  }

  private def findTopLevelClass(name: String): Validation[RuntimeClass] = {
    val currentPackage = declaringType.map(_.name.reverse.dropWhile(_ != '.').reverse).toOption
    val fqcns = getAllFullyQualifiedClassNames(name).toSet -- Set("scala.Boolean")
    val candidates = currentPackage match {
      case Some(currentPackage) if fqcns.size > 1 =>
        // if there is ambiguity we prefer the class in the current package
        val candidates = fqcns.filter(_.startsWith(currentPackage))
        if (candidates.size > 0) candidates else fqcns
      case _ => fqcns
    }
    candidates
      .validateSingle(s"Cannot find top-level class $name")
      .flatMap(loadClass)
      .map(StaticOrTopLevelClass.apply)
  }

  private def getAllFullyQualifiedClassNames(name: String): Seq[String] = {
    val scalaClassName = NameTransformer.scalaClassName(name)
    val encodedName = NameTransformer.encode(name)
    sourceLookUp.classesByScalaName(scalaClassName).filter(fqcn => fqcn.endsWith(encodedName))
  }

  private def findMemberOrModule(name: String, qualifier: RuntimeEvaluationTree): Validation[RuntimeEvaluationTree] =
    findField(qualifier, name)
      .orElse(findZeroArgMethod(qualifier, name))
      .orElse(findModule(name, qualifier))
      .orElse(findOuter(qualifier).flatMap(findMemberOrModule(name, _)))

  private def findModule(name: String, qualifier: RuntimeEvaluationTree): Validation[RuntimeEvaluationTree] = {
    val moduleClassName = toModuleName(name)
    val qualifierTypeName = qualifier.`type`.name
    if (inCompanion(qualifierTypeName, moduleClassName))
      Recoverable(s"Cannot access module $name from $qualifierTypeName")
    else findMemberClass(moduleClassName, qualifier).flatMap(cls => asModule(cls.`type`, qualifier))
  }

  private def inCompanion(qualifierTypeName: String, moduleClassName: String) =
    qualifierTypeName.endsWith("$") &&
      loadClass(qualifierTypeName.stripSuffix("$"))
        .withFilter(_.methodsByName(moduleClassName.stripSuffix("$")).asScala.nonEmpty)
        .isValid

  private def toModuleName(name: String) = if (name.endsWith("$")) name else name + "$"
  private def findTopLevelModule(name: String): Validation[RuntimeEvaluationTree] =
    findTopLevelClass(toModuleName(name)).flatMap(cls => asStaticModule(cls.`type`))

  private def findTopLevelModule(pkg: String, name: String): Validation[RuntimeEvaluationTree] =
    findQualifiedClass(toModuleName(name), pkg).flatMap(asStaticModule)

  private def findStaticMember(name: String, qualifier: jdi.ReferenceType): Validation[RuntimeEvaluationTree] =
    findStaticField(qualifier, name).orElse(findZeroArgStaticMethod(qualifier, name))

  /* Standardized method call */
  private case class Call(fun: Term, args: Seq[RuntimeEvaluationTree])

  private def standardize(apply: Stat): Validation[Call] =
    apply match {
      case apply: Term.Apply => apply.argClause.map(validateAsValue).traverse.map(Call(apply.fun, _))
      case Term.ApplyInfix.After_4_6_0(lhs, op, _, argClause) if op.value.endsWith(":") =>
        validateAsValue(lhs).map(arg => Call(Term.Select(argClause.head, op), List(arg)))
      case apply: Term.ApplyInfix =>
        validateAsValue(apply.argClause(0)).map(arg => Call(Term.Select(apply.lhs, apply.op), Seq(arg)))
      case apply: Term.ApplyUnary => Valid(Call(Term.Select(apply.arg, Term.Name("unary_" + apply.op)), List.empty))
    }

  private def validateMethod(call: Call): Validation[RuntimeEvaluationTree] = {
    call.fun match {
      case Term.Select(qualifier, Name(name)) =>
        validateAsValueOrClass(qualifier)
          .flatMap {
            case Left(qualifier) =>
              logger.info(s"args: ${call.args.map(_.`type`).mkString(", ")}")
              asPrimitiveOp(qualifier, name, call.args)
                .orElse(
                  findMethodBySignedName(qualifier, name, call.args),
                  resetError = isReference(qualifier) || call.args.size > 2
                )
                .orElseIf(qualifier.`type`.name() == "String")({logger.info("here"); catchStringOpsMethods(qualifier, name, call.args)})
            case Right(qualifier) => findStaticMethodBySignedName(qualifier, name, call.args)
          }
          .orElse(validateAsValue(call.fun).flatMap(findApplyMethod(_, call.args)))
      case Term.Name(name) =>
        thisTree
          .flatMap(findMethodInThisOrOuter(_, name, call.args))
          .orElse(validateAsValue(call.fun).flatMap(findApplyMethod(_, call.args)))
          .orElse(declaringType.flatMap(findStaticMethodBySignedName(_, name, call.args)))
    }
  }

  private def findApplyMethod(
      qualifier: RuntimeEvaluationTree,
      args: Seq[RuntimeEvaluationTree]
  ): Validation[RuntimeEvaluationTree] =
    findMethodBySignedName(qualifier, "apply", args).orElse(asArrayElem(qualifier, args))

  private def findMethodInThisOrOuter(
      thisOrOuter: RuntimeEvaluationTree,
      name: String,
      args: Seq[RuntimeEvaluationTree]
  ): Validation[RuntimeEvaluationTree] =
    asPrimitiveOp(thisOrOuter, name, args)
      .orElse(findMethodBySignedName(thisOrOuter, name, args), resetError = true)
      .orElse(findOuter(thisOrOuter).flatMap(findMethodInThisOrOuter(_, name, args)))

  private def asPrimitiveOp(
      lhs: RuntimeEvaluationTree,
      name: String,
      args: Seq[RuntimeEvaluationTree]
  ): Validation[RuntimeEvaluationTree] =
    args.toList match {
      case Nil => UnaryOp(lhs.`type`, name).map(CallUnaryOp(lhs, _))
      case rhs :: Nil => BinaryOp(lhs.`type`, rhs.`type`, name).map(CallBinaryOp(lhs, rhs, _))
      case _ => Recoverable(s"$name is not a primitive operation")
    }

  private def validateNew(newValue: Term.New): Validation[RuntimeEvaluationTree] = {
    val tpe = newValue.init.tpe
    val argClauses = newValue.init.argClauses
    for {
      args <- argClauses.flatMap(_.map(validateAsValue)).traverse
      cls <- findClass(tpe)
      allArgs = extractCapture(cls).toSeq ++ args
      init <- findBestMethodBySignedName(cls.`type`, "<init>", allArgs.map(_.`type`))
    } yield NewInstance(CallStaticMethod(init, allArgs, cls.`type`))
  }

  // ! May not be correct when dealing with an object inside a class
  private def findOuter(qualifier: RuntimeEvaluationTree): Validation[RuntimeEvaluationTree] = {
    val qualifierTypeName = qualifier.`type`.name
    asReference(qualifier.`type`)
      .flatMap(tpe => Validation(tpe.fieldByName("$outer")))
      .flatMap(asInstanceField(_, qualifier, preevaluate = true))
      .orElse {
        Validation
          .fromOption(removeLastInnerTypeFromFQCN(qualifierTypeName), s"$qualifierTypeName is not an inner class")
          .flatMap(name => loadClass(name + "$"))
          .flatMap(asStaticModule)
      }
  }

  private def validateIf(tree: Term.If): Validation[RuntimeEvaluationTree] =
    for {
      cond <- validateAsValue(tree.cond)
      thenp <- validateAsValue(tree.thenp)
      elsep <- validateAsValue(tree.elsep)
      ifTree <- asIfTree(cond, thenp, elsep)
    } yield ifTree

  private def asIfTree(
      cond: RuntimeEvaluationTree,
      ifTrue: RuntimeEvaluationTree,
      ifFalse: RuntimeEvaluationTree
  ): Validation[RuntimeEvaluationTree] = {
    val tType = ifTrue.`type`
    val fType = ifFalse.`type`

    if (isBoolean(cond.`type`)) {
      val tpe =
        if (isAssignableFrom(tType, fType)) Valid(tType)
        else if (isAssignableFrom(fType, tType)) Valid(fType)
        else getCommonSuperClass(tType, fType).orElse(loadClass("java.lang.Object"))
      tpe.map(tpe => preEvaluate(If(cond, ifTrue, ifFalse, tpe)))
    } else Recoverable("The condition of a if must be a boolean")
  }

  private def isBoolean(tpe: jdi.Type): Boolean =
    tpe != null && (tpe.isInstanceOf[jdi.BooleanType] || tpe.name == "java.lang.Boolean")

  private def validateAssign(tree: Term.Assign): Validation[RuntimeEvaluationTree] = {
    val lhs =
      tree.lhs match {
        case Term.Select(qualifier, Name(name)) =>
          validateAsValueOrClass(qualifier).flatMap {
            case Left(qualifier) => findField(qualifier, name, false)
            case Right(qualifier) => findStaticField(qualifier, name, false)
          }
        case Term.Name(name) =>
          findVariable(name, false)
            .orElse(thisTree.flatMap(findField(_, name, false)))
            .orElse(declaringType.flatMap(findStaticField(_, name, false)))
        case _ => Recoverable("Unsupported assignment")
      }

    for {
      lhs <- lhs
        .collect { case a: Assignable if a.isMutable => a }
      rhs <- validateAsValue(tree.rhs)
        .filter("Cannot assign value")(rhs => isAssignableFrom(rhs.`type`, lhs.`type`))
    } yield Assign(lhs, rhs, unitTree.get.`type`)
  }

  private def isReference(tree: RuntimeEvaluationTree): Boolean =
    tree.`type`.isInstanceOf[jdi.ReferenceType]

  private def asReference(tpe: jdi.Type): Validation[jdi.ReferenceType] =
    tpe match {
      case tpe: jdi.ReferenceType => Valid(tpe)
      case _ => Recoverable(s"$tpe is not a reference type")
    }

  private def moreSpecificThan(m1: jdi.Method, m2: jdi.Method): Boolean = {
    m1.argumentTypes()
      .asScala
      .zip(m2.argumentTypes.asScala)
      .forall {
        case (t1, t2) if nameOrNull(t1) == nameOrNull(t2) => true
        case (_: jdi.PrimitiveType, _) | (_, _: jdi.PrimitiveType) => true
        case (r1: jdi.ReferenceType, r2: jdi.ReferenceType) => isAssignableFrom(r1, r2)
      }
  }

  /**
   * @see <a href="https://docs.oracle.com/javase/specs/jls/se20/html/jls-15.html#jls-15.12.2.5">JLS#15.12.2.5. Choosing the most specific method</a>
   *
   * @param methods the list of compatible methods to compare
   * @return a sequence containing the most precise methods
   */
  private def filterMostPreciseMethod(methods: Iterable[jdi.Method]): Seq[jdi.Method] =
    methods.foldLeft(List.empty[jdi.Method]) { (m1, m2) =>
      m1 match {
        case Nil => List(m2)
        case list =>
          list.flatMap { m =>
            if (moreSpecificThan(m, m2)) List(m)
            else if (moreSpecificThan(m2, m)) List(m2)
            else List(m, m2)
          }.distinct
      }
    }

  /**
   * Look for a method with the given name and arguments types, on the given reference type
   * If multiple methods are found, a [[Validation.Unrecoverable]] is returned
   *
   * It loads and prepares the method return type, if needed.
   *
   * @param ref the reference type on which to look for the method
   * @param encodedName the name of the method
   * @param args the arguments types of the method
   * @return the method, wrapped in a [[Validation]]
   */
  private def findBestMethodBySignedName(
      ref: jdi.ReferenceType,
      name: String,
      args: Seq[jdi.Type]
  ): Validation[jdi.Method] = {
    val candidates = findMethodsByName(ref, name)
    logger.debug(s"Found methods: ${candidates.mkString("\n -> ", "\n -> ", "\n")}")
    logger.debug(s"Provided args type: ${args.map(nameOrNull).mkString("[", ", ", "]")}")
    val unboxedCandidates = candidates.filter(matchArguments(_, args, boxing = false))
    val boxedCandidates = unboxedCandidates.size match {
      case 0 => candidates.filter(matchArguments(_, args, boxing = true))
      case _ => unboxedCandidates
    }
    val withoutBridges = boxedCandidates.size match {
      case 0 | 1 => boxedCandidates
      case _ => boxedCandidates.filterNot(_.isBridge())
    }
    val finalCandidates = withoutBridges.size match {
      case 0 | 1 => withoutBridges
      case _ => filterMostPreciseMethod(withoutBridges)
    }
    def formatArgs = args.map(nameOrNull).mkString("[", ", ", "]")
    finalCandidates
      .validateSingle(s"Cannot find method $name with arguments of types $formatArgs in ${ref.name}")
      .map(loadClassOnNeed)
  }

  private def findZeroArgMethod(qualifier: RuntimeEvaluationTree, name: String): Validation[CallMethod] =
    asReference(qualifier.`type`)
      .flatMap {
        findBestMethodBySignedName(_, name, Seq())
          .filterNot("Accessing a module from its instanciation method is not allowed")(isModuleCall)
          .flatMap(asInstanceMethod(_, Seq.empty, qualifier))
      }
      .orElse(catchStringOpsMethods(qualifier, name, Seq.empty))

  private def findZeroArgStaticMethod(qualifier: jdi.ReferenceType, name: String): Validation[CallStaticMethod] =
    findBestMethodBySignedName(qualifier, name, Seq()).flatMap { m =>
      if (isModuleCall(m)) Recoverable("Accessing a module from its instanciation method is not allowed")
      else asStaticMethod(m, Seq.empty, qualifier)
    }

  private def isModuleCall(m: jdi.Method): Boolean = {
    val rt = m.returnTypeName
    val noArgs = m.argumentTypeNames.size == 0
    val isSingleton = rt.endsWith("$")
    val isSingletonInstantiation = rt.stripSuffix("$").endsWith(m.name)
    noArgs && isSingleton && isSingletonInstantiation
  }

  private def findMethodsByName(ref: jdi.ReferenceType, name: String): Seq[jdi.Method] = {
    val encodedName = if (name == "<init>" && ref.isInstanceOf[jdi.ClassType]) name else NameTransformer.encode(name)
    ref.methodsByName(encodedName).asScala.toSeq
  }

  private def findMethodBySignedName(
      qualifier: RuntimeEvaluationTree,
      name: String,
      args: Seq[RuntimeEvaluationTree]
  ): Validation[RuntimeEvaluationTree] = {
    val ownMethod = if (!args.isEmpty) {
      for {
        qualifierType <- asReference(qualifier.`type`)
        method <- findBestMethodBySignedName(qualifierType, name, args.map(_.`type`))
        methodTree <- asInstanceMethod(method, args, qualifier)
      } yield methodTree
    } else findZeroArgMethod(qualifier, name)

    ownMethod.orElseIf(qualifier.`type`.name() == "String")(catchStringOpsMethods(qualifier, name, args))
  }

  private def catchStringOpsMethods(qual: RuntimeEvaluationTree, name: String, args: Seq[RuntimeEvaluationTree]) =
    for {
      so <- findClass("StringOps")
      init <- findBestMethodBySignedName(so.`type`, "<init>", Seq(qual.`type`))
      initTree = NewInstance(CallStaticMethod(init, Seq(qual), so.`type`))
      _ = logger.debug(
        s"Looking for method $name on ${initTree.`type`} with args: ${args.map(_.`type`).mkString(", ")}"
      )
      stringOpMethod <- findBestMethodBySignedName(initTree.`type`, name, args.map(_.`type`))
      _ = logger.debug(s"Found method $stringOpMethod")
    } yield CallInstanceMethod(stringOpMethod, args, initTree)

  private def findStaticMethodBySignedName(
      qualifier: jdi.ReferenceType,
      name: String,
      args: Seq[RuntimeEvaluationTree]
  ): Validation[CallStaticMethod] =
    if (!args.isEmpty)
      findBestMethodBySignedName(qualifier, name, args.map(_.`type`)).flatMap(asStaticMethod(_, args, qualifier))
    else findZeroArgStaticMethod(qualifier, name)

  private def extractCapture(cls: RuntimeClass): Option[RuntimeEvaluationTree] =
    cls match {
      case MemberClass(tpe, qualifier) =>
        val capturesOuter = tpe
          .methodsByName("<init>")
          .asScala
          .filter(init => init.declaringType.name == tpe.name)
          .forall { init =>
            init.argumentTypeNames.asScala.headOption
              .exists { argType =>
                val suffix = toModuleName(argType)
                tpe.name.startsWith(suffix) && tpe.name.size > suffix.size
              }
          }
        if (capturesOuter) Some(qualifier) else None
      case StaticOrTopLevelClass(tpe) => None
    }

  private def isAssignableFrom(got: jdi.Type, expected: jdi.Type): Boolean = {
    def referenceTypesMatch(got: jdi.ReferenceType, expected: jdi.ReferenceType) = {
      val assignableFrom = expected.classObject.referenceType.methodsByName("isAssignableFrom").get(0)
      val params = Seq(got.classObject).asJava
      expected.classObject
        .invokeMethod(frame.thread, assignableFrom, params, jdi.ObjectReference.INVOKE_SINGLE_THREADED)
        .asInstanceOf[jdi.BooleanValue]
        .value()
    }

    (got, expected) match {
      case (g: jdi.ArrayType, at: jdi.ArrayType) =>
        checkClassStatus(at.componentType)
        g.componentType.equals(at.componentType)
      case (g: jdi.PrimitiveType, pt: jdi.PrimitiveType) => got.equals(pt)
      case (g: jdi.ReferenceType, ref: jdi.ReferenceType) => referenceTypesMatch(g, ref)
      case (_: jdi.VoidType, _: jdi.VoidType) => true
      case (g: jdi.ReferenceType, pt: jdi.PrimitiveType) => isAssignableFrom(g, frame.getPrimitiveBoxedClass(pt))
      case (g: jdi.PrimitiveType, ct: jdi.ReferenceType) => isAssignableFrom(frame.getPrimitiveBoxedClass(g), ct)
      case (null, _: jdi.ReferenceType) => true
      case _ => false
    }
  }

  private def matchArguments(method: jdi.Method, args: Seq[jdi.Type], boxing: Boolean): Boolean =
    method.argumentTypeNames.size == args.size &&
      method.argumentTypes.asScala.zip(args).forall {
        case (_: jdi.PrimitiveType, _: jdi.ReferenceType) if !boxing => false
        case (_: jdi.ReferenceType, _: jdi.PrimitiveType) if !boxing => false
        case (expected, got) => isAssignableFrom(got, expected)
      }

  private def loadClass(name: String): Validation[jdi.ClassType] =
    Validation.fromTry(frame.classLoader().flatMap(_.loadClass(name)).extract(_.cls))

  private def checkClassStatus(tpe: => jdi.Type) = Try(tpe) match {
    case Failure(e: jdi.ClassNotLoadedException) => loadClass(e.className())
    case Success(value: jdi.ClassType) if !value.isPrepared => loadClass(value.name)
    case result => result
  }

  private def loadClassOnNeed[T <: jdi.TypeComponent](tc: T): T = {
    def tpe = tc match {
      case field: jdi.Field => field.`type`
      case method: jdi.Method => method.returnType
    }
    val name = tc match {
      case field: jdi.Field => field.typeName
      case method: jdi.Method => method.returnTypeName
    }
    checkClassStatus(tpe)
    tc
  }

  // ! TO REFACTOR :sob:
  private def resolveInnerType(qualifier: jdi.Type, name: String): Validation[jdi.ReferenceType] = {
    var current: Validation[jdi.ReferenceType] = Recoverable(s"Cannot find outer class for $qualifier")
    def loop(qualifier: jdi.Type): Validation[jdi.ReferenceType] =
      qualifier match {
        case _: jdi.ArrayType | _: jdi.PrimitiveType | _: jdi.VoidType =>
          Recoverable("Cannot find outer class on non reference type")
        case ref: jdi.ReferenceType =>
          val loadedCls = loadClass(concatenateInnerTypes(ref.name, name))
          if (loadedCls.isValid) loadedCls
          else {
            var superTypes: List[jdi.ReferenceType] = ref match {
              case cls: jdi.ClassType => cls.superclass() :: cls.interfaces().asScala.toList
              case itf: jdi.InterfaceType => itf.superinterfaces().asScala.toList
            }

            while (!superTypes.isEmpty && !current.isValid) {
              val res = loop(superTypes.head)
              if (res.isValid) current = res
              else superTypes = superTypes.tail
            }
            current
          }
      }

    loop(qualifier)
  }

  private def getCommonSuperClass(tpe1: jdi.Type, tpe2: jdi.Type): Validation[jdi.Type] = {
    def getSuperClasses(tpe: jdi.Type): Array[jdi.ClassType] =
      tpe match {
        case cls: jdi.ClassType =>
          Iterator.iterate(cls)(cls => cls.superclass).takeWhile(_ != null).toArray
        case _ => Array()
      }

    val superClasses1 = getSuperClasses(tpe1)
    val superClasses2 = getSuperClasses(tpe2)
    Validation.fromOption(
      superClasses1.find(superClasses2.contains),
      s"${nameOrNull(tpe1)} and ${nameOrNull(tpe2)} do not have any common super class"
    )
  }

  private def findClass(tpe: scala.meta.Type): Validation[RuntimeClass] =
    tpe match {
      case scala.meta.Type.Name(name) => findClass(name)
      case scala.meta.Type.Select(qualifier, Name(name)) =>
        validateAsValueOrClass(qualifier)
          .flatMap {
            case Left(qualifier) => findMemberClass(name, qualifier)
            case Right(qualifier) => findStaticClass(name, qualifier)
          }
          .orElse(findQualifiedClass(name, qualifier.toString).map(StaticOrTopLevelClass.apply))
      case tpe => Recoverable(s"Cannot create instance of $tpe")
    }

  private def removeLastInnerTypeFromFQCN(className: String): Option[String] = {
    val (packageName, clsName) = className.splitAt(className.lastIndexOf('.') + 1)
    val name = NameTransformer.decode(clsName)
    val lastDollar = name.stripSuffix("$").lastIndexOf('$')
    lastDollar match {
      case -1 => None
      case _ => Some(packageName + name.dropRight(name.length - lastDollar))
    }
  }

  private def concatenateInnerTypes(className: String, innerName: String): String =
    if (className.endsWith("$")) className + innerName
    else className + "$" + innerName

  private def isStaticModule(tpe: jdi.Type): Boolean = tpe match {
    case tpe: jdi.ClassType => tpe.fieldByName("MODULE$") != null
    case _ => false
  }

  private def asStaticModule(tpe: jdi.Type): Validation[RuntimeEvaluationTree] =
    if (isStaticModule(tpe)) Valid(preEvaluate(StaticModule(tpe.asInstanceOf[jdi.ClassType])))
    else Recoverable(s"${nameOrNull(tpe)} is not a static module")

  private def asModule(tpe: jdi.ReferenceType, qualifier: RuntimeEvaluationTree): Validation[RuntimeEvaluationTree] =
    if (isStaticModule(tpe)) asStaticModule(tpe)
    else {
      val objectName = NameTransformer.scalaClassName(tpe.name).stripSuffix("$")
      asReference(qualifier.`type`)
        .flatMap(findBestMethodBySignedName(_, objectName, Seq()))
        .map(m => preEvaluate(NestedModule(tpe, CallInstanceMethod(m, Seq.empty, qualifier))))
    }

  private def asInstanceField(
      field: jdi.Field,
      qualifier: RuntimeEvaluationTree,
      preevaluate: Boolean
  ): Validation[RuntimeEvaluationTree] = {
    def instanceField =
      if (preevaluate) preEvaluate(InstanceField(field, qualifier))
      else InstanceField(field, qualifier)
    if (isStaticModule(field.`type`)) asStaticModule(field.`type`)
    else if (isStaticModule(qualifier.`type`)) Valid(instanceField)
    else if (field.isStatic) Recoverable(s"Cannot access static field ${field.name} from instance of a class")
    else Valid(instanceField)
  }

  private def asStaticField(field: jdi.Field, preevaluate: Boolean): Validation[RuntimeEvaluationTree] =
    if (field.isStatic) Valid(if (preevaluate) preEvaluate(StaticField(field)) else StaticField(field))
    else Recoverable(s"Cannot access instance field ${field.name} from static context")

  private def asInstanceMethod(
      method: jdi.Method,
      args: Seq[RuntimeEvaluationTree],
      qualifier: RuntimeEvaluationTree
  ): Validation[CallMethod] =
    if (method.isStatic) Recoverable(s"Cannot access static method ${method.name} from instance of a class")
    else Valid(CallInstanceMethod(method, args, qualifier))

  private def asStaticMethod(
      method: jdi.Method,
      args: Seq[RuntimeEvaluationTree],
      qualifier: jdi.ReferenceType
  ): Validation[CallStaticMethod] =
    if (method.isStatic) Valid(CallStaticMethod(method, args, qualifier))
    else Recoverable(s"Cannot access instance method ${method.name} from static context")

  private def asArrayElem(
      array: RuntimeEvaluationTree,
      args: Seq[RuntimeEvaluationTree]
  ): Validation[RuntimeEvaluationTree] = {
    val integerTypes = Seq("java.lang.Integer", "java.lang.Short", "java.lang.Byte", "java.lang.Character")
    if (args.size != 1) Recoverable("Array accessor must have one argument")
    else {
      val index = args.head
      array.`type` match {
        case arrayTpe: jdi.ArrayType =>
          index.`type` match {
            case (_: jdi.IntegerType | _: jdi.ShortType | _: jdi.ByteType | _: jdi.CharType) =>
              Valid(preEvaluate(new ArrayElem(array, index, arrayTpe.componentType)))
            case ref: jdi.ReferenceType if integerTypes.contains(ref.name) =>
              Valid(preEvaluate(new ArrayElem(array, index, arrayTpe.componentType)))
            case tpe => Recoverable(s"Array index must be an integer, found ${tpe.name}")
          }
        case tpe => Recoverable(s"${tpe.name} is not an array")
      }
    }
  }

  private def nameOrNull(tpe: jdi.Type): String =
    if (tpe == null) "null" else tpe.name

  private implicit class IterableExtensions[A](iter: Iterable[A]) {
    def validateSingle(message: String): Validation[A] =
      iter.size match {
        case 1 => Valid(iter.head)
        case 0 => Recoverable(message)
        case _ => Recoverable(s"$message: more than one candidate")
      }
  }
}
