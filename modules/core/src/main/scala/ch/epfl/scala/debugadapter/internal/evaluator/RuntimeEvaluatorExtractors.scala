package ch.epfl.scala.debugadapter.internal.evaluator

import scala.meta.Term
import com.sun.jdi._

protected[internal] object RuntimeEvaluatorExtractors {
  object ColonEndingInfix {
    def unapply(stat: Term.ApplyInfix): Option[Term.ApplyInfix] =
      stat match {
        case Term.ApplyInfix.After_4_6_0(_, op, _, _) if (op.value.endsWith(":")) => Some(stat)
        case _ => None
      }
  }

  object Module {
    def unapply(tpe: Type): Option[ClassType] =
      tpe match {
        case ref: ClassType if ref.fieldByName("MODULE$") != null => Some(ref)
        case _ => None
      }

    def unapply(cls: JdiClass): Option[ClassType] = unapply(cls.cls)

    def unapply(tree: RuntimeTree): Option[RuntimeEvaluableTree] =
      unapply(tree.`type`).map(_ => tree.asInstanceOf[RuntimeEvaluableTree])
  }

  object MethodCall {
    def unapply(tree: RuntimeTree): Option[RuntimeTree] =
      tree match {
        case mt: NestedModuleTree => unapply(mt.of)
        case ft: InstanceFieldTree => unapply(ft.qual)
        case oct: OuterClassTree => unapply(oct.inner)
        case OuterModuleTree(module) => unapply(module)
        case _: MethodTree | _: NewInstanceTree => Some(tree)
        case _: LiteralTree | _: LocalVarTree | _: PreEvaluatedTree | _: ThisTree => None
        case _: StaticFieldTree | _: ClassTree | _: TopLevelModuleTree => None
        case _: PrimitiveBinaryOpTree | _: PrimitiveUnaryOpTree | _: ArrayElemTree => None
      }
    def unapply(tree: Validation[RuntimeTree]): Option[RuntimeTree] =
      tree.toOption.filter { unapply(_).isDefined }
  }

  object ReferenceTree {
    def unapply(tree: RuntimeTree): Validation[ReferenceType] = {
      tree.`type` match {
        case ref: ReferenceType => Valid(ref)
        case _ => Recoverable(s"$tree is not a reference type")
      }
    }

    def unapply(tree: Validation[RuntimeTree]): Validation[ReferenceType] =
      tree.flatMap(unapply)
  }

  object IsAnyVal {
    def unapply(x: Any): Option[AnyVal] = x match {
      case _: Byte | _: Short | _: Char | _: Int | _: Long | _: Float | _: Double | _: Boolean =>
        Some(x.asInstanceOf[AnyVal])
      case _ => None
    }
  }

  object PrimitiveTest {
    object IsIntegral {
      def unapply(x: Type): Boolean = x match {
        case _: ByteType | _: ShortType | _: CharType | _: IntegerType | _: LongType => true
        case _ =>
          if (AllowedReferences.integrals.contains(x.name)) true
          else false
      }
    }
    object IsFractional {
      def unapply(x: Type): Boolean = x match {
        case _: FloatType | _: DoubleType => true
        case _ =>
          if (AllowedReferences.fractionals.contains(x.name)) true
          else false
      }
    }
    object IsNumeric {
      def unapply(x: Type): Boolean = x match {
        case IsIntegral() | IsFractional() => true
        case _ =>
          if (AllowedReferences.numerics.contains(x.name)) true
          else false
      }
    }
    object NotNumeric { def unapply(x: Type): Boolean = !IsNumeric.unapply(x) }
    object IsBoolean {
      def unapply(x: Type): Boolean = x match {
        case _: BooleanType => true
        case _ =>
          if (AllowedReferences.booleans.contains(x.name)) true
          else false
      }
    }
    object IsPrimitive {
      def unapply(x: Type): Boolean = x match {
        case IsNumeric() | IsBoolean() => true
        case _ => false
      }
    }
    object NotPrimitive { def unapply(x: Type): Boolean = !IsPrimitive.unapply(x) }
  }
}

/* -------------------------------------------------------------------------- */
/*                          Allowed types reference                           */
/* -------------------------------------------------------------------------- */
private object AllowedReferences {
  val integrals = Set(
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Short",
    "java.lang.Byte"
  )
  val extendedIntegrals = integrals + "java.lang.Character"
  val fractionals = Set(
    "java.lang.Float",
    "java.lang.Double"
  )
  val numerics = extendedIntegrals ++ fractionals
  val booleans = Set("java.lang.Boolean")
  val allReferences = numerics ++ booleans
}
