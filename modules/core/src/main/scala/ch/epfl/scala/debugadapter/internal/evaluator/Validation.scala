package ch.epfl.scala.debugadapter.internal.evaluator

import java.util.NoSuchElementException

import com.sun.jdi.*
import ch.epfl.scala.debugadapter.Logger

sealed abstract class Validation[+A] {
  def isValid: Boolean
  def isEmpty: Boolean = !isValid

  def filter(message: => String)(p: A => Boolean): Validation[A]
  def filterNot(message: => String)(p: A => Boolean): Validation[A] = filter(message)(!p(_))
  def withFilter(p: A => Boolean): Validation[A] = filter("predicate does not hold")(p)

  def map[B](f: A => B)(implicit logger: Logger): Validation[B]
  def flatMap[B](f: A => Validation[B])(implicit logger: Logger): Validation[B]

  def tap(f: A => Unit): Validation[A]
  def collect[B](pf: PartialFunction[A, B])(implicit logger: Logger): Validation[B] =
    if (pf.isDefinedAt(get)) Valid(pf(get)) else Recoverable(s"Value did not match the predicate")

  def get: A
  def getOrElse[B >: A](f: => B): B
  def orElse[B >: A](f: => Validation[B], resetError: Boolean = false): Validation[B]
  def orElseIf[B >: A](p: => Boolean)(f: => Validation[B], resetError: Boolean = false): Validation[B] =
    if (isValid) this else if (p) orElse(f, resetError) else this

  def toOption: Option[A]
}

final case class Valid[+A](value: A) extends Validation[A]() {
  override val isValid: Boolean = true
  override def map[B](f: A => B)(implicit logger: Logger): Validation[B] = Validation(f(value))
  override def flatMap[B](f: A => Validation[B])(implicit logger: Logger): Validation[B] = f(value)
  def tap(f: A => Unit): Validation[A] = {
    f(get)
    this
  }
  override def get = value
  override def getOrElse[B >: A](f: => B): B = value
  override def orElse[B >: A](f: => Validation[B], resetError: Boolean): Validation[B] = this

  override def filter(message: => String)(p: A => Boolean): Validation[A] =
    if (p(value)) this else Recoverable(message)

  override def toOption: Option[A] = Some(value)
}

sealed trait Invalid extends Validation[Nothing] {
  def exception: Throwable
  override val isValid: Boolean = false
  override def map[B](f: Nothing => B)(implicit logger: Logger): Validation[B] = this
  override def flatMap[B](f: Nothing => Validation[B])(implicit logger: Logger): Validation[B] = this
  def tap(f: Nothing => Unit): Validation[Nothing] = this
  override def get = throw exception
  override def getOrElse[B >: Nothing](f: => B): B = f

  override def filter(message: => String)(p: Nothing => Boolean): Validation[Nothing] = this

  override def toOption: Option[Nothing] = None
}

final case class Recoverable(exception: Exception) extends Invalid {
  override def orElse[B >: Nothing](f: => Validation[B], resetError: Boolean): Validation[B] = f match {
    case valid: Valid[B] => valid
    case other: Recoverable => if (resetError) other else this
    case fatal: Fatal => fatal
  }
}

final case class Fatal(exception: Throwable) extends Invalid {
  override def orElse[B >: Nothing](f: => Validation[B], resetError: Boolean): Validation[B] = this
}

object Validation {
  private def handler(t: Throwable)(implicit logger: Logger): Invalid = t match {
    case e @ (_: VMDisconnectedException | _: ObjectCollectedException) => Fatal(e)
    case e @ (_: InvalidStackFrameException | _: AbsentInformationException) => Fatal(e)
    case e @ (_: InvocationException | _: VMOutOfMemoryException) => Fatal(e)
    case e: Exception =>
      logger.warn(s"Unexpected error while validating: $e")
      Recoverable(e)
  }

  def apply[A](input: => A)(implicit logger: Logger): Validation[A] = {
    try {
      val value = input
      if (value == null) Recoverable("Found null value, expected non-null value")
      else Valid(value)
    } catch {
      case t: Throwable => handler(t)
    }
  }

  def fromOption[A](value: => Option[A], message: String): Validation[A] = {
    value match {
      case Some(value) => Valid(value)
      case None => Recoverable(new NoSuchElementException(message))
    }
  }

  def fromTry[A](value: => scala.util.Try[A])(implicit logger: Logger): Validation[A] = {
    value match {
      case scala.util.Success(value) => Valid(value)
      case scala.util.Failure(t) => handler(t)
    }
  }

}

object Invalid {
  def unapply(invalid: Invalid): Option[Throwable] = Some(invalid.exception)
}

object Recoverable {
  def apply(s: String) = new Recoverable(new NoSuchElementException(s))
}
