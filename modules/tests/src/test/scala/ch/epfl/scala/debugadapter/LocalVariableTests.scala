package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.testfmk.*

class LocalVariableTests extends DebugTestSuite {
  val scalaVersion = ScalaVersion.`3.1+`

  test("Should set the right expression for array elements") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val array = Array(1, 2, 3)
         |    println("ok")
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      LocalVariable.inspect("array")(
        _.forall(v => """array\(\d+\)""".r.unapplySeq(v.evaluateName).isDefined)
      )
    )
  }

  test("simple local variables") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = 1
         |    println(x)
         |    val y = "2"
         |    println(y)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      LocalVariable()(Seq("args", "x", "this")),
      Breakpoint(8),
      LocalVariable()(Seq("args", "x", "y", "this")),
      LocalVariable("y", "CASE_INSENSITIVE_ORDER")(Nil)
    )
  }

  test("tailLocal variables") { // no tailLocal
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    @annotation.tailrec
         |    def factAcc(x: Int, acc: Int): Int =
         |      if x <= 1 then acc
         |      else factAcc(x - 1, x * acc)
         |    println(factAcc(5, 1))
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7),
      LocalVariable()(Seq("x", "acc", "this")),
      Breakpoint(8),
      LocalVariable()(Seq("x", "acc", "this"))
    )
  }

  test("SAMorPartialFunctionImpl") { // no z
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val xs = List(1, 2, 3)
         |    xs.collect { 
         |      case z if z % 2 == 0 => z 
         |    }
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      LocalVariable()(Seq("args", "this")),
      Breakpoint(6),
      LocalVariable()(Seq("args", "xs", "this")),
      Breakpoint(7),
      LocalVariable()(Seq("args", "xs", "this")),
      Breakpoint(7),
      LocalVariable()(Seq("x", "default", "this"))
    )
  }

  test("inlined this") { // check A_this why it's not formatted
    val source =
      """|package example
         |
         |class A(x: Int):
         |  inline def foo: Int = x + x
         |
         |object Main {
         |  def bar(a: A) = a.foo
         |
         |  def main(args: Array[String]): Unit = {
         |    println(bar(new A(1)))
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7),
      LocalVariable()(Nil),
      Breakpoint(7),
      LocalVariable()(Seq("a", "this")),
      Breakpoint(7),
      LocalVariable()(Seq("a", "A_this", "this"))
    )
  }

  test("inlined param") {
    val source =
      """|package example
         |
         |object Main {
         |  inline def foo(x: Int): Int = x + x
         |
         |  def bar(y: Int) = 
         |    val z = foo(y + 2)
         |    z
         |
         |  def main(args: Array[String]): Unit = {
         |    println(bar(2))
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7),
      LocalVariable()(Seq("y", "this")),
      Breakpoint(7),
      LocalVariable()(Seq("y", "x", "this")),
      Breakpoint(8),
      LocalVariable()(Seq("y", "z", "this"))
    )
  }

  test("lazy val capture") {
    val source =
      """|package example
         |
         |object Main {
         |  def foo =
         |    val y = 4
         |    lazy val z = y + 1
         |    def bar = 
         |      z
         |    bar
         |  def main(args: Array[String]): Unit = {
         |    println(foo)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      LocalVariable()(Seq("y", "this")),
      Breakpoint(8),
      LocalVariable()(Seq("z", "y", "this"))
    )
  }

  test("by-name arg capture") { // no x$1
    val source =
      """|package example
         |
         |object Main {
         |  def foo(x: => Int) = 
         |    val y = x
         |    y
         |
         |  def bar(y: Int) = 
         |    foo(y)
         |
         |  def main(args: Array[String]): Unit = {
         |    val y = 1
         |    println(bar(y))
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      LocalVariable()(Seq("y", "this"))
    )
  }

  test("binds") {
    val source =
      """|package example
         |
         |class B
         |case class C(x: Int, y: String) extends B
         |case class D(z: String) extends B
         |case class E(v: Int) extends B
         |case class F(w: Int) extends B
         |
         |class A:
         |  def bar(a: B) =
         |    a match
         |      case F(w) => 
         |        w
         |      case C(x, y) =>
         |        x
         |      case D(z) => 0
         |      case E(v) => 1
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    println(a.bar(F(3)))
         |    println(a.bar(C(1, "2")))
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(13),
      LocalVariable()(Seq("a", "w", "this")),
      Breakpoint(15),
      LocalVariable()(Seq("a", "x", "y", "this"))
    )
  }

  test("mixin and trait static forwarders") { // $this?
    val source =
      """|package example
         |
         |trait A {
         |  def foo(x: Int): Int = 
         |    x
         |}
         |
         |class B extends A
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val b = new B
         |    println(b.foo(1))
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      LocalVariable()(Seq("x", "this"))
    )
  }

  test("this AnyVal") { // ??
    val source =
      """|package example
         |
         |class A(x: Int) extends AnyVal {
         |  def foo: Int = 
         |    x
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A(1)
         |    println(a.foo)
         |    println(a)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      LocalVariable()(Seq("x", "this"))
    )
  }

  test("this variable") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println(4)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      LocalVariable()(Seq("args", "this"))
    )
  }

  test("binds tuple and pattern matching") {
    val source =
      """|package example
         |
         |object Main {
         |  def foo: Int = 
         |    val x = (1, 2)
         |    val (c, d) = (3, 4)
         |    x match
         |      case (a, b) => 
         |        a + b
         |      case null => 0
         |
         |  def main(args: Array[String]): Unit = {
         |    println(foo)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(8),
      LocalVariable()(Seq("x", "c", "d", "this")),
      Breakpoint(9),
      LocalVariable()(Seq("x", "c", "d", "a", "b", "this"))
    )
  }

  test("ambiguous variables 2") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    var i = 0
         |    while (i < 1) {
         |      val x = i
         |      i += 1
         |    }
         |    val x = 17
         |    println(x)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(8),
      LocalVariable()(Seq("args", "i", "x", "this")),
      Breakpoint(11),
      LocalVariable()(Seq("args", "i", "x", "this"))
    )
  }

  test("ambiguous variables") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = true
         |    if (a) {
         |      val x = 1
         |      println(x)
         |    }
         |    val x = "2"
         |    println(x)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7),
      LocalVariable()(Seq("args", "a", "this")),
      Breakpoint(8),
      LocalVariable()(Seq("args", "a", "x", "this")),
      Breakpoint(11),
      LocalVariable()(Seq("args", "a", "x", "this"))
    )
  }

  test("local object") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    object B
         |    println(B)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      LocalVariable()(Seq("args", "this"))
    )
  }

  test("local lazy val") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    lazy val x = 1
         |    println(x)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      LocalVariable()(Seq("args", "this"))
    )
  }

  test("array") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = Array(1, 2, 3)
         |    println(x)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      LocalVariable()(Seq("args", "x", "this"))
    )
  }

  test("captured param in a local def") {
    val source =
      """|package example
         |
         |object Main {
         |  def foo(x: Int) =
         |    def bar = 
         |      x
         |    bar
         |
         |  def main(args: Array[String]): Unit = {
         |    val y = 1
         |    println(foo(y))
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      LocalVariable()(Seq("x", "this"))
    )
  }
}
