package nz.rd.whiley

final class Relation(
    val posAndPos: Ternary,
    val posAndNeg: Ternary,
    val negAndPos: Ternary,
    val negAndNeg: Ternary
) {
  def flip: Relation = new Relation(posAndPos, negAndPos, posAndNeg, negAndNeg)
  def unary_!(): Relation = new Relation(negAndPos, negAndNeg, posAndPos, posAndNeg)
  def |(that: Relation): Relation = new Relation(
    posAndPos | that.posAndPos,
    posAndNeg | that.posAndNeg,
    negAndPos | that.negAndPos,
    negAndNeg | that.negAndNeg
  )
  def &(that: Relation): Relation = new Relation(
    posAndPos & that.posAndPos,
    posAndNeg & that.posAndNeg,
    negAndPos & that.negAndPos,
    negAndNeg & that.negAndNeg
  )
  def posOrPos: Ternary = !negAndNeg
  def posOrNeg: Ternary = !negAndPos
  def negOrPos: Ternary = !posAndNeg
  def negOrNeg: Ternary = !posAndPos
  def equal: Ternary = !negAndPos & !posAndNeg
  def disjoint: Ternary = !posAndPos
  def implies: Ternary = !posAndNeg
}

object Relation {
  val Same = new Relation(TUnknown, TFalse, TFalse, TUnknown)
  val Complement = new Relation(TFalse, TFalse, TFalse, TTrue)
  val Unknown = new Relation(TUnknown, TUnknown, TUnknown, TUnknown)
}