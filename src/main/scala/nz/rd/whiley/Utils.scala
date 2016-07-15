package nz.rd.whiley

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future}

object Utils {

  /**
   * Iterate on mutable state until a fixpoint is reached.
   */
  @tailrec
  def fixpoint(readState: => Any)(body: => Any): Unit = {
    val before = readState
    body
    val after = readState
    if (before != after) { fixpoint(readState)(body) }
  }

  def pairwise[A](elements: Iterable[A]): Vector[(A, A)] = {
    @tailrec
    def headsAndTails(as: Iterable[A], acc: Vector[(A, Iterable[A])]): Vector[(A, Iterable[A])] = {
      if (as.isEmpty) acc else {
        val t = as.tail
        headsAndTails(t, acc :+ (as.head, t))
      }
    }
    headsAndTails(elements, Vector.empty).flatMap {
      case (head, tail) => tail.map((head, _))
    }
  }

  sealed trait Iteration[+B]
  case class Continue[+B](value: B) extends Iteration[B]
  case class Break[+B](value: B) extends Iteration[B]


  def breakableFold[A, B](in: Iterable[A], z: B)(f: (B, A) => Iteration[B]): B = {
    val iterator = in.iterator

    @tailrec
    def loop(iteration: Iteration[B]): B = iteration match {
      case Break(value) => value
      case Continue(value) =>
        if (iterator.hasNext) {
          val element = iterator.next()
          val elementIteration = f(value, element)
          loop(elementIteration)
        } else {
          value
        }
    }

    loop(Continue(z))
  }
}