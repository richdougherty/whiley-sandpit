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

    // TODO: Make tail recursive?
    def addTree(t: Tree, id: Id = a.freshId()): Unit = {
      t match {
        case Tree.Int =>
          a(id) = Node.Int
        case Tree.Negation(child) =>
          val childId: Id = a.freshId()
          a(id) = Node.Negation(childId)
          addTree(child, childId)
        case Tree.Union(children) =>
          val childrenIds: List[Id] = children.map(_ => a.freshId())
          a(id) = Node.Union(childrenIds)
          // Not tail recursive
          for ((child, childId) <- (children zip childrenIds)) {
            addTree(child, childId)
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
  }
}