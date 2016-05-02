package nz.rd.whiley

import scala.collection.mutable

final class Graph private[Graph] (
    nodes: mutable.Map[Graph.Id, Graph.Node],
    private var nextId: Graph.Id) {
  def freshId(): Graph.Id = {
    val id = nextId
    nextId += 1
    id
  }
  def apply(id: Graph.Id): Graph.Node = nodes(id)
  def update(id: Graph.Id, n: Graph.Node): Unit = nodes.update(id, n)
  def +=(entry: (Graph.Id, Graph.Node)): Unit = {
    nodes += entry
  }
  def +=(n: Graph.Node): Graph.Id = {
    val id = freshId()
    this +=(id, n)
    id
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