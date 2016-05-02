package nz.rd.whiley

import scala.collection.mutable

final class Graph private[Graph] (
    var root: Graph.Id,
    val nodes: mutable.Map[Graph.Id, Graph.Node],
    private var nextId: Graph.Id) {

  override def toString: String = s"Graph($root, $nodes)"

  import Graph.{Id, Node}

  def freshId(): Id = {
    val id = nextId
    nextId += 1
    id
  }
  def apply(id: Id): Node = nodes(id)
  def update(id: Id, n: Node): Unit = nodes.update(id, n)
  def +=(entry: (Id, Node)): Unit = {
    nodes += entry
  }
  def +=(n: Node): Id = {
    val id = freshId()
    this +=(id, n)
    id
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
            case Node.Int => ()
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
}

object Graph {
  type Id = Int

  def fromTree(tree: Tree): Graph = {

    val g = new Graph(0, mutable.Map.empty, 0)

    def convert(t: Tree, nameBindings: Map[String, Id]): Id = {
      t match {
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
    final case object Int extends Node
    final case class Negation(child: Id) extends Node
    final case class Union(children: List[Id]) extends Node
    final case class Record(fields: List[(String,Id)]) extends Node
  }
}