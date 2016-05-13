package nz.rd.whiley

sealed trait Tree
object Tree {
  final case object Any extends Tree
  final case object Void extends Tree
  final case object Null extends Tree
  final case object Int extends Tree
  final case class Negation(child: Tree) extends Tree
  final case class Union(children: List[Tree]) extends Tree
  final case class Record(fields: List[(String,Tree)]) extends Tree
  final case class Recursive(name: String, body: Tree) extends Tree
  final case class Variable(name: String) extends Tree
}