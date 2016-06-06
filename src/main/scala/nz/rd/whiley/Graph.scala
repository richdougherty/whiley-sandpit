package nz.rd.whiley

import scala.collection.mutable

final class Graph private[Graph] (
    var root: Graph.Id,
    val nodes: mutable.Map[Graph.Id, Graph.Node],
    var nextId: Graph.Id) {

  override def toString: String = s"Graph($root, $nodes)"

  import Graph.{Id, Node}

  def freshId(): Id = {
    val id = nextId
    nextId += 1
    id
  }
  def apply(id: Id): Node = nodes(id)
  def update(id: Id, n: Node): Unit = {
    // !!!! DEBUGGING !!!!
    // if (n == Node.Any) {
    //   nodes.get(id).foreach {
    //     case Node.Void | Node.Null | Node.Int =>
    //       throw new Exception(s"Tried to replace node $id with Any in $this")
    //     case _ => ()
    //   }
    // }
    // !!!! END DEBUGGING !!!!
    nodes.update(id, n)
  }
  def +=(entry: (Id, Node)): Unit = {
    nodes += entry
  }
  def +=(n: Node): Id = {
    val id = freshId()
    this +=(id, n)
    id
  }

  def copy: Graph = {
    val other = Graph.empty
    other.root = root
    other.nodes ++= nodes
    other.nextId = nextId
    other
  }

  def toTree: Tree = {

    sealed trait Recursion
    final case object NotRecursive extends Recursion
    final case class WillBind(name: String) extends Recursion
    final case class BoundTo(name: String) extends Recursion

    var recursiveNameCount = 0
    val recursionMap = mutable.Map[Id, Recursion]()

    def findRecursion(id: Id): Unit = {
      recursionMap.get(id) match {
        case None =>
          recursionMap(id) = NotRecursive
          apply(id) match {
            case Node.Any | Node.Void | Node.Int | Node.Null => ()
            case Node.Negation(nodeChild) => findRecursion(nodeChild)
            case Node.Union(nodeChildren) => nodeChildren.foreach(findRecursion)
            case Node.Record(nodeFields) => nodeFields.foreach {
              case (_, fieldNode) => findRecursion(fieldNode)
            }
          }
        case Some(NotRecursive) =>
          val name = "X"+recursiveNameCount
          recursiveNameCount += 1
          recursionMap(id) = WillBind(name)
        case Some(WillBind(_)) =>
          // Already done
        case Some(BoundTo(_)) =>
          throw new AssertionError("Recursive types should not be bound yet")
      }
    }
    findRecursion(root)

    def convert(id: Id): Tree = {

      def simpleConvert(id: Id): Tree = {
        apply(id) match {
          case Node.Any => Tree.Any
          case Node.Void => Tree.Void
          case Node.Null => Tree.Null
          case Node.Int => Tree.Int
          case Node.Negation(nodeChild) => Tree.Negation(convert(nodeChild))
          case Node.Union(nodeChildren) => Tree.Union(nodeChildren.map(convert))
          case Node.Record(nodeFields) => Tree.Record(nodeFields.map {
            case (name, fieldNode) => (name, convert(fieldNode))
          })
        }
      }

      recursionMap.get(id) match {
        case None =>
          throw new AssertionError("All types should have known recursion");
        case Some(NotRecursive) =>
          simpleConvert(id)
        case Some(WillBind(name)) =>
          recursionMap(id) = BoundTo(name)
          Tree.Recursive(name, simpleConvert(id))
        case Some(BoundTo(name)) =>
          Tree.Variable(name)
      }
    }
    convert(root)
  }

  def prune(): Unit = {
    val reachableNodes = mutable.Set[Id]()

    def markReachable(id: Id): Unit = {
      val alreadyMarked = reachableNodes.contains(id)
      if (!alreadyMarked) {
        reachableNodes += id // Mark self
        val node = nodes(id)
        node match {
          case Node.Union(children) =>
            children.foreach(markReachable(_))
          case Node.Negation(child) =>
            markReachable(child)
          case Node.Record(fields) =>
            fields.map(_._2).foreach(markReachable(_))
          case _ =>
            () // Don't need to do anything for nodes without children
        }
      }
    }
    markReachable(root)

    for (id <- nodes.keysIterator) {
      if (!reachableNodes.contains(id)) {
        nodes -= id
      }
    }
  }
}

object Graph {
  type Id = Int

  def empty: Graph = new Graph(0, mutable.Map.empty, 0)

  def apply(root: Int, nodes: Map[Id, Node]): Graph = {
    val g = empty
    g.nodes ++= nodes
    g.root = root
    g.nextId = (nodes.keys.foldLeft(0) { case (maxSoFar, id) => Math.max(maxSoFar, id) }) + 1
    g
  }

  def fromTree(tree: Tree): Graph = {

    val g = empty

    def convert(t: Tree, nameBindings: Map[String, Id]): Id = {
      t match {
        case Tree.Any =>
          g += Node.Any
        case Tree.Void =>
          g += Node.Void
        case Tree.Null =>
          g += Node.Null
        case Tree.Int =>
          g += Node.Int
        case Tree.Negation(child) =>
          g += Node.Negation(convert(child, nameBindings))
        case Tree.Union(children) =>
          g += Node.Union(children.map(convert(_, nameBindings)))
        case Tree.Record(treeFields) =>
          g += Node.Record(treeFields.map {
            case (name, child) => (name, convert(child, nameBindings))
          })
        case Tree.Recursive(name, body) =>
          val tempId = g.freshId()
          val bodyId = convert(body, nameBindings + (name -> tempId))
          assert(bodyId != tempId, "Recursive type with no body")
          def replaceId(id: Id): Id = if (id == tempId) bodyId else id
          for ((id, node) <- g.nodes) {
            val newNode = node match {
              case Node.Any => Node.Any
              case Node.Void => Node.Void
              case Node.Null => Node.Null
              case Node.Int => Node.Int
              case Node.Negation(c) => Node.Negation(replaceId(c))
              case Node.Union(cs) =>
                Node.Union(cs.map(replaceId))
              case Node.Record(fs) => Node.Record(fs.map {
                case (name, fieldId) => (name, replaceId(fieldId))
              })
            }
            g(id) = newNode
          }
          bodyId
        case Tree.Variable(name) =>
          nameBindings(name)
      }
    }
    g.root = convert(tree, Map.empty)
    g
  }

  sealed trait Node
  object Node {
    final case object Any extends Node
    final case object Void extends Node
    final case object Null extends Node
    final case object Int extends Node
    final case class Negation(child: Id) extends Node
    final case class Union(children: List[Id]) extends Node
    final case class Record(fields: List[(String,Id)]) extends Node
  }
}