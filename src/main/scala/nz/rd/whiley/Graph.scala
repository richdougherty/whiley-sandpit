package nz.rd.whiley

import scala.collection.mutable

final class Graph private[Graph] (
    nodes: mutable.Map[Graph.Id, Graph.Node],
    private var nextId: Graph.Id) {

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
    def convert(id: Id): Tree = {
      apply(id) match {
        case Node.Int => Tree.Int
        case Node.Negation(nodeChild) => Tree.Negation(convert(nodeChild))
        case Node.Union(nodeChildren) => Tree.Union(nodeChildren.map(convert))
        case Node.Record(nodeFields) => Tree.Record(nodeFields.map {
          case (name, fieldNode) => (name, convert(fieldNode))
        })
      }
    }
    convert(0)
  }
}

object Graph {
  type Id = Int

  def fromTree(tree: Tree): Graph = {

    val a = new Graph(mutable.Map.empty, 0)

    def addTree(t: Tree, id: Id = a.freshId()): Unit = {
      t match {
        case Tree.Int =>
          a(id) = Node.Int
        case Tree.Negation(treeChild) =>
          val nodeChild: Id = a.freshId()
          a(id) = Node.Negation(nodeChild)
          addTree(treeChild, nodeChild)
        case Tree.Union(treeChildren) =>
          val nodeChildren: List[Id] = treeChildren.map(_ => a.freshId())
          a(id) = Node.Union(nodeChildren)
          (treeChildren zip nodeChildren).foreach {
            case (childTree, childId) => addTree(childTree, childId)
          }
        case Tree.Record(treeFields) =>
          val nodeFields: List[(String, Id)] = treeFields.map {
            case (fieldName, fieldTree) =>
              val fieldId = a.freshId()
              (fieldName, fieldId)
          }
          a(id) = Node.Record(nodeFields)
          (treeFields zip nodeFields) foreach {
            case ((_, fieldTree), (_, fieldId)) => addTree(fieldTree, fieldId)
          }
      }
    }
    addTree(tree)
    a
  }

  sealed trait Node
  object Node {
    final case object Int extends Node
    final case class Negation(child: Id) extends Node
    final case class Union(children: List[Id]) extends Node
    final case class Record(fields: List[(String,Id)]) extends Node
  }
}