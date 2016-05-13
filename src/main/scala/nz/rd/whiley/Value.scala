package nz.rd.whiley

sealed trait Value
object Value {
  final case class Int(x: scala.Int) extends Value
  final case class Record(fields: List[(String,Value)]) extends Value
}