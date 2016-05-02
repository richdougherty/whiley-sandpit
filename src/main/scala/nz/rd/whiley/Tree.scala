package nz.rd.whiley

sealed trait Tree
object Tree {
  final case object Int extends Tree
  final case class Negation(child: Tree) extends Tree
  final case class Union(children: List[Tree]) extends Tree
  final case class Record(fields: List[(String,Tree)]) extends Tree
}