package nz.rd.whiley

sealed trait Ternary {
  def &(t: Ternary): Ternary
  def |(t: Ternary): Ternary
  def unary_!(): Ternary
  def isTrue: Boolean
  def isFalse: Boolean
  def isUnknown: Boolean
}

final case object TTrue extends Ternary {
  override def &(t: Ternary): Ternary = t
  override def |(t: Ternary): Ternary = TTrue
  override def unary_!(): Ternary = TFalse
  override def toString: String = "1"
  override def isTrue: Boolean = true
  override def isFalse: Boolean = false
  override def isUnknown: Boolean = false
}

final case object TFalse extends Ternary {
  def &(t: Ternary): Ternary = TFalse
  def |(t: Ternary): Ternary = t
  override def unary_!(): Ternary = TTrue
  override def toString: String = "0"
  override def isTrue: Boolean = false
  override def isFalse: Boolean = true
  override def isUnknown: Boolean = false
}

final case object TUnknown extends Ternary {
  def &(t: Ternary): Ternary = t match {
    case TFalse => TFalse
    case _ => t
  }
  def |(t: Ternary): Ternary = t match {
    case TTrue => TTrue
    case _ => t
  }
  override def unary_!(): Ternary = TUnknown
  override def toString: String = "?"
  override def isTrue: Boolean = false
  override def isFalse: Boolean = false
  override def isUnknown: Boolean = true
}