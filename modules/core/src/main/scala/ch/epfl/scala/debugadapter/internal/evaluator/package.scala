package ch.epfl.scala.debugadapter.internal

import scala.util.Try

package object evaluator {
  implicit class SafeSeq[A](seq: Seq[Safe[A]]) {
    def traverse: Safe[Seq[A]] = {
      seq.foldRight(Safe(Seq.empty[A])) { (safeHead, safeTail) =>
        safeTail.flatMap(tail => safeHead.map(head => head +: tail))
      }
    }
  }

  implicit class SafeOption[A](opt: Option[Safe[A]]) {
    def traverse: Safe[Option[A]] =
      opt.map(s => s.map(Option.apply)).getOrElse(Safe(None))
  }

  implicit class TryToSafe[A](t: Try[A]) {
    def toSafe: Safe[A] = Safe(t)
  }
}
