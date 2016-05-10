package nz.rd.whiley

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable
import scala.reflect.ClassTag
import nz.rd.whiley.Graph.{ Id, Node }

final class IntersectionAlgorithm(
    val g: Graph,
    val contents: mutable.Map[Id, Contents],
    val intersections: mutable.Map[(Id, Id), Intersections]) {

  def getInts(a: Id, b: Id): Intersections = {
    // Normalise key order
    if (a <= b) {
      intersections.getOrElseUpdate((a, b), Intersections())
    } else {
      // Look up with flipped keys, then use flipped view of intersection
      intersections.getOrElseUpdate((b, a), Intersections()).flipped
    }
  }

  def getContents(a: Id): Contents = {
    contents.getOrElseUpdate(a, new Contents())
  }

  /**
   * Iterate on mutable state until a fixpoint is reached.
   */
  @tailrec
  private def fixpoint(readState: => Any)(body: => Any): Unit = {
    val before = readState
    body
    val after = readState
    if (before != after) { fixpoint(readState)(body) }
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

  abstract class Has[N <: Node : ClassTag] {
    def unapply(p: (Id, Id)): Option[(Intersections, Id, N, Id)] = {
      def test(a: Id, b: Id): Option[(Intersections, Id, N, Id)] = {
        val cls: Class[N] = implicitly[ClassTag[N]].runtimeClass.asInstanceOf[Class[N]]
        val n: Node = g(a)
        if (cls.isInstance(n)) {
          val ints: Intersections = getInts(a, b)
          Some((ints, a, n.asInstanceOf[N], b))
        } else {
          None
        }
      }
      test(p._1, p._2) orElse test(p._2, p._1)
    }
  }
  object Has {
    object Any extends Has[Node.Any.type]
    object Void extends Has[Node.Void.type]
    object Union extends Has[Node.Union]
    object Negation extends Has[Node.Negation]
  }

  object AnyNodes {
    def unapply(p: (Id, Id)): Option[(Intersections, Id, Node, Id, Node)] = {
      val ints: Intersections = getInts(p._1, p._2)
      val n1: Node = g(p._1)
      val n2: Node = g(p._2)
      Some((ints, p._1, n1, p._2, n2))
    }
  }

  def calculate(): Unit = {

    fixpoint {
      (g.root, g.nodes.toList)
    } {

      // Simple, structural simplifications

      fixpoint {
        (g.root, g.nodes.toList)
      } {
        for ((id, node) <- g.nodes) {
          node match {
            case Node.Any => ()
            case Node.Void => ()
            case Node.Int => ()
            case Node.Union(children) =>
              if (children.exists(g.nodes(_) == Node.Any)) {
                // If there's an Any node, then replace Union with Any
                g.nodes(id) = Node.Any
              } else {
                // Process and combine nested unions, removing inter-union links
                val unions = mutable.Set[Id]()
                def tidy(childrenToTidy: List[Id]): List[Id] = {
                  childrenToTidy.map(cid => (cid, g.nodes(cid))).flatMap {
                    case (cid, _) if unions.contains(cid) =>
                      // Union already processed
                      Seq.empty
                    case (_, Node.Void) =>
                      // Void node can be omitted
                      Seq.empty
                    case (cid, Node.Union(childChildren)) =>
                      // Add union to set of processed unions; merge its children
                      unions += cid
                      tidy(childChildren)
                    case (cid, _) =>
                      Seq(cid)
                  }
                }
                unions += id
                val newChildren = tidy(children)
                val newNode = newChildren match {
                  case Nil => Node.Void
                  case child::Nil => g.nodes(child)
                  case _ => Node.Union(newChildren)
                }
                g.nodes(id) = newNode
              }
            case Node.Negation(child) =>
              val childNode = g.nodes(child)
              childNode match {
                case Node.Any =>
                  g(id) = Node.Void
                case Node.Void =>
                  g(id) = Node.Any
                case Node.Negation(grandchild) =>
                  // Flatten double negation
                  g(id) = g(grandchild)
                case _ =>
                  ()
              }
          }
        }
      }

      // Calculate emptiness of nodes and their negations

      fixpoint {
        contents.toList.map {
          case (key, contents) => (key, (contents.p, contents.n))
        }
      } {
        for ((id, node) <- g.nodes) {
          val conts = getContents(id)
          node match {
            case Node.Any =>
              conts.p = Some(NonEmpty)
              conts.n = Some(Empty)
            case Node.Void =>
              conts.p = Some(Empty)
              conts.n = Some(NonEmpty)
            case Node.Int =>
              conts.p = Some(NonEmpty)
              conts.n = Some(NonEmpty)
            case Node.Union(children) =>
              val childrenContent = children.map(getContents(_))
              if (childrenContent.exists(_.p == Some(NonEmpty))) { conts.p = Some(NonEmpty) }
              if (childrenContent.forall(_.n == Some(Empty))) { conts.n = Some(Empty) }
            case Node.Negation(child) =>
              val childContent = getContents(child)
              conts.p = childContent.n
              conts.n = childContent.p
          }
        }
      }

      // Calculate emptiness of intersections between nodes and their negations

      fixpoint {
        intersections.toList.map {
          case (key, ints) => (key, (ints.pp, ints.pn, ints.np, ints.nn))
        }
      } {
        for (((a: Id, an: Node), (b: Id, bn: Node)) <- pairs(g.nodes)) {
          (a, b) match {

            // Types with nodes that are the same

            case AnyNodes(ints, aid, anode, bid, bnode) if anode == bnode =>
              val ints = getInts(a, b)
              val contents = getContents(a)
              ints.pp = contents.p
              ints.pn = Some(Empty)
              ints.np = Some(Empty)
              ints.nn = contents.n

            // Types with intersections derived from their children

            case Has.Negation(ints, id, Node.Negation(c), oid) =>
              val childInts = getInts(c, oid)
              ints.pp = childInts.np
              ints.pn = childInts.nn
              ints.np = childInts.pp
              ints.nn = childInts.pn
            case Has.Union(ints, id, Node.Union(children), oid) =>
              val childrenInts = children.map(getInts(_, oid))
              if (childrenInts.exists(_.pp == Some(NonEmpty))) { ints.pp = Some(NonEmpty) }
              if (childrenInts.exists(_.pn == Some(NonEmpty))) { ints.pn = Some(NonEmpty) }
              if (childrenInts.forall(_.np == Some(Empty))) { ints.np = Some(Empty) }
              if (childrenInts.forall(_.nn == Some(Empty))) { ints.nn = Some(Empty) }

            // Types with intersections derived from the other term

            case Has.Any(ints, id, Node.Any, oid) =>
              val otherContents = getContents(id)
              ints.pp = otherContents.p
              ints.pn = otherContents.n
              ints.np = Some(Empty)
              ints.nn = Some(Empty)
            case Has.Void(ints, id, Node.Void, oid) =>
              val otherContents = getContents(id)
              ints.pp = Some(Empty)
              ints.pn = Some(Empty)
              ints.np = otherContents.p
              ints.nn = otherContents.n

            case _ =>
              throw new MatchError(s"Failed to match $a (${g(a)}) and $b (${g(b)})")
          }
        }
      }

      // TODO: Use intersection information to find equal types

      // Use intersection information to simplify unions

      fixpoint {
        (g.root, g.nodes.toList)
      } {
        for ((id, node) <- g.nodes) {
          node match {
            case Node.Union(children) =>
              val removals = mutable.ArrayBuffer[Id]()
              val additions = mutable.ArrayBuffer[Id]()
              val f: ((Id, Id)) => Unit = {
                case (a, b) if a == b =>
                  // Don't process self-intersections
                  ()
                case AnyNodes(ints, aid, anode, bid, bnode) =>
                  (ints.pp, ints.pn, ints.np, ints.nn) match {
                    case (_, _, _, Some(Empty)) => // union of both sets is Any
                      removals += aid
                      removals += bid
                      additions += (g += Node.Any)
                    case (_, _, Some(Empty), _) => // a > b - remove b
                      removals += bid
                    case (_, Some(Empty), _, _) => // b > a - remove a
                      removals += aid
                  }
                  ()
              }
              pairs(children).foreach(f)
              val newChildren = children.filter(c => !removals.contains(c)) ++ additions
              g(id) = Node.Union(newChildren)

            case _ => ()
          }
        }
      }
    }

  }

}

// sealed trait PairMatch
// case class SelfMatch(id: Id) extends PairMatch
// case class OneMatch(id: Id, ints: Intersections, node: Node, other: Id) extends PairMatch

object IntersectionAlgorithm {
  def forGraph(g: Graph) = new IntersectionAlgorithm(g, mutable.Map.empty, mutable.Map.empty)
}

trait Intersections {
  var pp: Option[Size]
  var pn: Option[Size]
  var np: Option[Size]
  var nn: Option[Size]
  override def toString: String = s"Intersections(pp: $pp, pn: $pn, np: $np, nn: $nn)"
  def flipped: Intersections
}

object Intersections {
  def apply(): Intersections = new CanonicalIntersections
  private class CanonicalIntersections extends Intersections { canonical =>
    override var pp: Option[Size] = None
    override var pn: Option[Size] = None
    override var np: Option[Size] = None
    override var nn: Option[Size] = None
    override def flipped: Intersections = new Intersections {
      override def flipped: Intersections = canonical
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

class Contents {
  var p: Option[Size] = None
  var n: Option[Size] = None
  override def toString: String = s"Contents(p: $p, n: $n)"
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
