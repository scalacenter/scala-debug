package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi
import ch.epfl.scala.debugadapter.internal.evaluator.RuntimePrimitiveOps.*

sealed trait RuntimeEvaluationTree {
  def `type`: jdi.Type
  override def toString(): String = prettyPrint(0)
  def prettyPrint(depth: Int): String
}

object RuntimeEvaluationTree {
  sealed trait CallMethod extends RuntimeEvaluationTree {
    def method: jdi.Method
    def args: Seq[RuntimeEvaluationTree]
  }

  sealed trait Assignable extends RuntimeEvaluationTree {
    def isMutable: Boolean
  }

  sealed trait Field extends Assignable {
    def field: jdi.Field
    override def isMutable: Boolean = !field.isFinal
  }

  case class LocalVar(name: String, `type`: jdi.Type) extends Assignable {
    override val isMutable: Boolean = true
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|LocalVar(
          |${indent}name= $name,
          |${indent}type= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class InstanceField(field: jdi.Field, qualifier: RuntimeEvaluationTree) extends Field {
    override lazy val `type` = field.`type`()
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|InstanceField(
          |${indent}field = $field,
          |${indent}qualifier = ${qualifier.prettyPrint(depth + 1)}
          |${indent.dropRight(1)})""".stripMargin
    }
  }
  case class StaticField(field: jdi.Field) extends Field {
    override lazy val `type` = field.`type`()
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|StaticField(
          |${indent}field = $field
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class CallInstanceMethod(
      method: jdi.Method,
      args: Seq[RuntimeEvaluationTree],
      qualifier: RuntimeEvaluationTree
  ) extends CallMethod {
    override lazy val `type` = method.returnType()
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|InstanceMethod(
          |${indent}method = $method -> ${method.returnType()},
          |${indent}args = ${args.map(_.prettyPrint(depth + 1)).mkString(",\n" + indent)},
          |${indent}qualifier = ${qualifier.prettyPrint(depth + 1)}
          |${indent.dropRight(1)})""".stripMargin
    }
  }
  case class CallStaticMethod(
      method: jdi.Method,
      args: Seq[RuntimeEvaluationTree],
      qualifier: jdi.ReferenceType
  ) extends CallMethod {
    override lazy val `type` = method.returnType()
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|CallStaticMethod(
          |${indent}m= $method,
          |${indent}args= ${args.map(_.prettyPrint(depth + 1)).mkString(",\n" + indent)},
          |${indent}qualifier= $qualifier
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class CallBinaryOp(
      lhs: RuntimeEvaluationTree,
      rhs: RuntimeEvaluationTree,
      op: BinaryOp
  ) extends RuntimeEvaluationTree {
    override lazy val `type` = op.typeCheck(lhs.`type`, rhs.`type`)
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|CallBinaryOp(
          |${indent}lhs = ${lhs.prettyPrint(depth + 1)},
          |${indent}rhs = ${rhs.prettyPrint(depth + 1)},
          |${indent}op = $op
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class ArrayElem(array: RuntimeEvaluationTree, index: RuntimeEvaluationTree, `type`: jdi.Type)
      extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|ArrayElem(
          |${indent}array = $array,
          |${indent}index = $index
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class CallUnaryOp(
      rhs: RuntimeEvaluationTree,
      op: UnaryOp
  ) extends RuntimeEvaluationTree {
    override lazy val `type` = op.typeCheck(rhs.`type`)
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|UnaryOpCall(
          |${indent}rhs= ${rhs.prettyPrint(depth + 1)},
          |${indent}op= $op
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class NewInstance(init: CallStaticMethod) extends RuntimeEvaluationTree {
    override lazy val `type`: jdi.ReferenceType = init.method.declaringType()
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|NewInstance(
          |${indent}init= ${init.prettyPrint(depth + 1)}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class This(`type`: jdi.ReferenceType) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = s"This(${`type`})"
  }

  case class StaticModule(`type`: jdi.ClassType) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|StaticModule(
          |${indent}mod= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class NestedModule(`type`: jdi.ReferenceType, init: CallInstanceMethod) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|NestedModule(
          |${indent}type = ${`type`}
          |${indent}init = ${init.prettyPrint(depth + 1)}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class Value(value: Safe[JdiValue], `type`: jdi.Type) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|Value(
          |${indent}v= $value,
          |${indent}t= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class Literal(value: Any, `type`: jdi.Type) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|Literal(
          |${indent}v= $value,
          |${indent}t= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class If(
      p: RuntimeEvaluationTree,
      thenp: RuntimeEvaluationTree,
      elsep: RuntimeEvaluationTree,
      `type`: jdi.Type
  ) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|If(
          |${indent}p= ${p.prettyPrint(depth + 1)},
          |${indent}ifTrue= ${thenp.prettyPrint(depth + 1)},
          |${indent}ifFalse= ${elsep.prettyPrint(depth + 1)}
          |${indent}t= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }

  case class Assign(
      lhs: Assignable,
      rhs: RuntimeEvaluationTree,
      `type`: jdi.Type
  ) extends RuntimeEvaluationTree {
    override def prettyPrint(depth: Int): String = {
      val indent = "  " * (depth + 1)
      s"""|Assign(
          |${indent}lhs= ${lhs.prettyPrint(depth + 1)},
          |${indent}rhs= ${rhs.prettyPrint(depth + 1)},
          |${indent}t= ${`type`}
          |${indent.dropRight(1)})""".stripMargin
    }
  }
}
