package nz.rd.whiley

sealed trait Value
object Value {
  final case object Null extends Value {
    override def toString = "null"
  }
  final case class Int(x: scala.Int) extends Value {
    override def toString = x.toString
  }
  final case class Product(vs: List[Value]) extends Value {
    override def toString = vs.mkString("<", ",", ">")
  }
}