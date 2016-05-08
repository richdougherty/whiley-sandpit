package nz.rd.whiley

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable
import scala.reflect.ClassTag
import nz.rd.whiley.Graph.{ Id, Node }

final class IntersectionAlgorithm(g: Graph, intersections: mutable.Map[(Id, Id), Intersections]) {

  def get(a: Id, b: Id): Intersections = {
    // Normalise key order
    val key: (Id, Id) = if (a <= b) (a, b) else (b, a)
    intersections(key)
  }

  private def pairs[A](seq: Iterable[A]): Iterable[(A, A)] = {
    def headsAndTails(as: Iterable[A], acc: Vector[(A, Iterable[A])]): Vector[(A, Iterable[A])] = {
      if (as.isEmpty) acc else {
        val t = as.tail
        headsAndTails(t, acc :+ (as.head, t))
      }
    }
    headsAndTails(seq, Vector.empty).flatMap {
      case (head, tail) => Vector((head, head)) ++ tail.map((head, _))
    }
  }

  for ((a, b) <- pairs(g.nodes.keys)) {
    val key: (Id, Id) = if (a <= b) (a, b) else (b, a)
    intersections(key) = Intersections()
  }

  abstract class Has[N <: Node : ClassTag] {
    def unapply(p: (Id, Id)): Option[(Intersections, Id, N, Id)] = {
      def test(a: Id, b: Id): Option[(Intersections, Id, N, Id)] = {
        val cls: Class[N] = implicitly[ClassTag[N]].runtimeClass.asInstanceOf[Class[N]]
        val n: Node = g(a)
        if (cls.isInstance(n)) {
          val ints: Intersections = get(a, b)
          Some((ints, a, n.asInstanceOf[N], b))
        } else {
          None
        }
      }
      test(p._1, p._2) orElse test(p._2, p._1)
    }
  }
  object Has {
    object Union extends Has[Node.Union]
    object Negation extends Has[Node.Negation]
  }

  def calculate(): Unit = {

    @tailrec
    def loop(): Unit = {
      def saveState: List[((Id, Id), (Option[Size], Option[Size], Option[Size], Option[Size]))] = {
        intersections.toList.map {
          case (key, ints) => (key, (ints.pp, ints.pn, ints.np, ints.nn))
        }
      }
      val before = saveState
      for (((a: Id, an: Node), (b: Id, bn: Node)) <- pairs(g.nodes)) {
        (a, b) match {
          case (_, _) if an == bn =>
            // TODO: Needs changing for any/void
            val ints = get(a, b)
            ints.pp = Some(NonEmpty)
            ints.pn = Some(Empty)
            ints.np = Some(Empty)
            ints.nn = Some(NonEmpty)
          case Has.Negation(ints, id, Node.Negation(c), oid) =>
            val childInts = get(c, oid)
            ints.pp = childInts.pp.map(_.inverse)
            ints.pn = childInts.pn.map(_.inverse)
            ints.np = childInts.np.map(_.inverse)
            ints.nn = childInts.nn.map(_.inverse)
          case Has.Union(ints, id, Node.Union(children), oid) =>
            val childrenInts = children.map(get(_, oid))
            if (childrenInts.exists(_.pp == Some(NonEmpty))) { ints.pp = Some(NonEmpty) }
            if (childrenInts.exists(_.pn == Some(NonEmpty))) { ints.pn = Some(NonEmpty) }
            if (childrenInts.forall(_.np == Some(Empty))) { ints.np = Some(Empty) }
            if (childrenInts.forall(_.nn == Some(Empty))) { ints.nn = Some(Empty) }
        }
      }
      val after = saveState
      if (before != after) { loop() } else {
        println(g.nodes)
        println(after)
      }
    }

    loop()
  }

}

trait Intersections {
  var pp: Option[Size]
  var pn: Option[Size]
  var np: Option[Size]
  var nn: Option[Size]
  def flip: Intersections
}

object Intersections {
  def apply(): Intersections = new CanonicalIntersections
  private class CanonicalIntersections extends Intersections { canonical =>
    override var pp: Option[Size] = None
    override var pn: Option[Size] = None
    override var np: Option[Size] = None
    override var nn: Option[Size] = None
    override def flip: Intersections = new Intersections {
      override def flip: Intersections = canonical
        def pp: Option[Size] = canonical.pp
        def pp_=(value: Option[Size]): Unit = canonical.pp = value
        def pn: Option[Size] = canonical.np
        def pn_=(value: Option[Size]): Unit = canonical.np = value
        def np: Option[Size] = canonical.pn
        def np_=(value: Option[Size]): Unit = canonical.pn = value
        def nn: Option[Size] = canonical.nn
        def nn_=(value: Option[Size]): Unit = canonical.nn = value
    }
  }
}

sealed trait Size {
  def inverse: Size
}
final case object Empty extends Size {
  override def inverse: Size = NonEmpty
}
final case object NonEmpty extends Size {
  override def inverse: Size = Empty
}
