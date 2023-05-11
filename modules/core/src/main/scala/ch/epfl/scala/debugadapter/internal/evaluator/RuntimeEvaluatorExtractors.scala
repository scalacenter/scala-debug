package ch.epfl.scala.debugadapter.internal.evaluator

import scala.meta.Term
import scala.meta.Defn
import scala.meta.Mod
import com.sun.jdi.{ClassType, Field, ReferenceType, Type}

private[internal] object RuntimeEvaluatorExtractors {
  object ColonEndingInfix {
    def unapply(stat: Term.ApplyInfix): Option[Term.ApplyInfix] =
      stat match {
        case Term.ApplyInfix.After_4_6_0(_, op, _, _) if (op.value.endsWith(":")) => Some(stat)
        case _ => None
      }
  }

  object LazyDefine {
    def unapply(stat: Defn.Val): Option[Defn.Val] =
      stat match {
        case Defn.Val(mods, _, _, _) if (mods.contains(Mod.Lazy)) => None
        case _ => Some(stat)
      }
  }

  object Module {
    def unapply(tpe: Type): Option[ClassType] =
      tpe match {
        case ref: ClassType if ref.fieldByName("MODULE$") != null => Some(ref)
        case _ => None
      }

    def unapply(field: Field): Option[ClassType] = unapply(field.`type`)

    def unapply(tree: RuntimeTree): Option[ClassType] =
      tree match {
        case mt: ModuleTree => Some(mt.`type`)
        case _: LiteralTree | _: LocalVarTree | _: NewInstanceTree | _: ClassTree | _: PrimitiveBinaryOpTree |
            _: PrimitiveUnaryOpTree =>
          None
        case _: FieldTree | _: ThisTree | _: MethodTree => unapply(tree.`type`)
      }
  }

  object Instance {
    def unapply(tree: RuntimeTree): Option[RuntimeEvaluationTree] =
      tree match {
        case _: ClassTree => None
        case _: ModuleTree => None
        case ft: FieldTree => Some(ft)
        case mt: MethodTree => Some(mt)
        case lit: LiteralTree => Some(lit)
        case lv: LocalVarTree => Some(lv)
        case th: ThisTree => Some(th)
        case pbt: PrimitiveBinaryOpTree => Some(pbt)
        case put: PrimitiveUnaryOpTree => Some(put)
        case nit: NewInstanceTree => Some(nit)
      }

    def unapply(tree: Option[RuntimeTree]): Option[RuntimeEvaluationTree] =
      if (tree.isEmpty) None
      else unapply(tree.get)
  }

  object MethodCall {
    def unapply(tree: Validation[RuntimeTree]): Option[RuntimeTree] =
      tree.toOption.filter {
        _ match {
          case mt: ModuleTree => mt.of.map(t => unapply(Valid(t))).isDefined
          case ft: InstanceFieldTree => unapply(Valid(ft.qual)).isDefined
          case _: MethodTree | _: NewInstanceTree => true
          case _: LiteralTree | _: LocalVarTree | _: ThisTree | _: StaticFieldTree | _: ClassTree |
              _: PrimitiveBinaryOpTree | _: PrimitiveUnaryOpTree =>
            false
        }
      }
  }

  object ReferenceTree {
    def unapply(tree: RuntimeTree): Validation[ReferenceType] = {
      tree.`type` match {
        case ref: ReferenceType => Valid(ref)
        case _ => Recoverable(s"$tree is not a reference type")
      }
    }

    def unapply(tree: Validation[RuntimeTree]): Validation[ReferenceType] = {
      if (tree.isInvalid) Recoverable("An invalid tree cannot be a reference type")
      else unapply(tree.get)
    }
  }

  object IsAnyVal {
    def unapply(x: Any): Option[AnyVal] = x match {
      case _: Byte | _: Short | _: Char | _: Int | _: Long | _: Float | _: Double | _: Boolean =>
        Some(x.asInstanceOf[AnyVal])
      case _ => None
    }
  }
}
