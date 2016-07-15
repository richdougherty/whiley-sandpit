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
  override def toString: String = "true"
  override def isTrue: Boolean = true
  override def isFalse: Boolean = false
  override def isUnknown: Boolean = false
}

final case object TFalse extends Ternary {
  def &(t: Ternary): Ternary = TFalse
  def |(t: Ternary): Ternary = t
  override def unary_!(): Ternary = TTrue
  override def toString: String = "false"
  override def isTrue: Boolean = false
  override def isFalse: Boolean = true
  override def isUnknown: Boolean = false
}

final case object TUnknown extends Ternary {
  def &(t: Ternary): Ternary = t match {
    case TFalse => TFalse
    case _ => this
  }
  def |(t: Ternary): Ternary = t match {
    case TTrue => TTrue
    case _ => this
  }
  override def unary_!(): Ternary = TUnknown
  override def toString: String = "unknown"
  override def isTrue: Boolean = false
  override def isFalse: Boolean = false
  override def isUnknown: Boolean = true
}