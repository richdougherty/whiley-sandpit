package nz.rd.whiley

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

sealed trait Shrub {
  def accepts(v: Value): Boolean
}
object Shrub {

  class Ref private (private var state: Ref.State) {
    def getRef: Ref = {
      @tailrec
      def loop(curr: Ref, prev: Ref.Indirection): Ref = {
        curr.state match {
          case i@Ref.Indirection(r) =>
            loop(r, i)
          case _ =>
            if (prev != null) { this.state = prev }
            curr
        }
      }
      loop(curr = this, prev = null)
    }
    def isEmpty: Boolean = getRef.state match {
      case Ref.Indirection(_) => throw new IllegalStateException("getRef shouldn't return another indirection")
      case Ref.HoldsShrub(_) => false
      case Ref.Empty => true
    }
    def get: Shrub = getRef.state match {
      case Ref.Indirection(_) => throw new IllegalStateException("getRef shouldn't return another indirection")
      case Ref.HoldsShrub(s) => s
      case Ref.Empty => throw new NoSuchElementException("Shrub.Ref is empty")
    }
    def set(s: Shrub): Unit = getRef.state = Ref.HoldsShrub(s)
    def link(r: Ref): Unit = getRef.state = Ref.Indirection(r)
    def clear(): Unit = getRef.state = Ref.Empty
    override def equals(that: Any): Boolean = that match {
      case thatRef: Ref => getRef eq thatRef.getRef
      case _ => false
    }
    override def hashCode: Int = System.identityHashCode(getRef)
    override def toString: String = {
      val visits = mutable.Map[Ref,Int]()
      var nextId = 0
      def refString(r: Ref): String = {
        visits.get(r) match {
          case None =>
            val id = nextId
            nextId += 1
            visits += (r -> id)
            if (r.isEmpty) "empty" else s"µX$id.(${shrubString(r.get)})"
          case Some(id) =>
            s"X$id"
        }
      }
      def shrubString(s: Shrub): String = s match {
        case Shrub.Any => "any"
        case Shrub.Void => "void"
        case Shrub.Int => "int"
        case Shrub.Null => "null"
        case Shrub.Negation(child) => s"!(${shrubString(child)})"
        case Shrub.Union(children) => children.map(shrubString(_)).mkString("(|", "|", ")")
        case Shrub.Intersection(children) => children.map(shrubString(_)).mkString("(&", "&", ")")
        case Shrub.Product(children) => children.map(refString(_)).mkString("<", ",", ">")
      }
      refString(this)
    }
  }

  object Ref {
    def empty(): Ref = new Ref(Empty)
    def apply(s: Shrub): Ref = new Ref(HoldsShrub(s))
    def apply(r: Ref): Ref = new Ref(Indirection(r))

    private[Ref] sealed trait State
    private[Ref] case object Empty extends State
    private[Ref] case class HoldsShrub(s: Shrub) extends State
    private[Ref] case class Indirection(r: Ref) extends State
  }

  final case object Any extends Shrub {
    override def accepts(v: Value): Boolean = true
  }
  final case object Void extends Shrub {
    override def accepts(v: Value): Boolean = false
  }
  final case object Null extends Shrub {
    override def accepts(v: Value): Boolean = v == Value.Null
  }
  final case object Int extends Shrub {
    override def accepts(v: Value): Boolean = v.isInstanceOf[Value.Int]
  }
  final case class Negation(child: Shrub) extends Shrub {
    override def accepts(v: Value): Boolean = !child.accepts(v)
  }
  final case class Union(children: List[Shrub]) extends Shrub {
    override def accepts(v: Value): Boolean = children.exists(_.accepts(v))
  }
  final case class Intersection(children: List[Shrub]) extends Shrub {
    override def accepts(v: Value): Boolean = children.forall(_.accepts(v))
  }

  final case class Product(children: List[Ref]) extends Shrub {
    override def accepts(v: Value): Boolean = v match {
      case Value.Product(valueChildren) =>
        children.length == valueChildren.length && (children zip valueChildren).forall { case (c, v) => c.get.accepts(v) }
      case _ => false
    }
  }

  def fromTree(tree: Tree): Ref = {
    val sy = fromTreeRaw(tree)
    ShrubberyAlgorithms.mergeIdentical(sy)
    sy
  }

  def fromTreeRaw(tree: Tree): Ref = {

    val treeRefs = mutable.Map.empty[Tree, Ref]

    def convertRoot(tree: Tree, namedTrees: Map[String, Tree]): Ref = {
      treeRefs.get(tree) match {
        case Some(id) =>
          id
        case None =>
          val ref = Shrub.Ref.empty() // Placeholder
          treeRefs += (tree -> ref)
          val shrub = convert(tree, Set.empty, namedTrees)
          ref.set(shrub)
          ref
      }
    }

    def convert(tree: Tree, visitedNames: immutable.Set[String], namedTrees: Map[String, Tree]): Shrub = {
      tree match {
        case Tree.Any => Shrub.Any
        case Tree.Void => Shrub.Void
        case Tree.Null => Shrub.Null
        case Tree.Int => Shrub.Int
        case Tree.Negation(child) =>
          Shrub.Negation(convert(child, visitedNames, namedTrees))
        case Tree.Union(children) =>
          Shrub.Union(children.map(convert(_, visitedNames, namedTrees)))
        case Tree.Intersection(children) =>
          Shrub.Intersection(children.map(convert(_, visitedNames, namedTrees)))
        case Tree.Product(children) =>
          Shrub.Product(children.map(convertRoot(_, namedTrees)))
        case rec@Tree.Recursive(name, body) =>
          convert(body, visitedNames + name, namedTrees + (name -> body))
        case Tree.Variable(name) if visitedNames.contains(name) => Shrub.Void
        case Tree.Variable(name) =>
          convert(namedTrees(name), visitedNames + name, namedTrees)
      }
    }
    convertRoot(tree, Map.empty)
  }
}


