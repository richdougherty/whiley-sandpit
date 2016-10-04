package nz.rd.whiley

final class Relation(
    val posAndPos: Ternary,
    val posAndNeg: Ternary,
    val negAndPos: Ternary,
    val negAndNeg: Ternary
) {
  def flip: Relation = new Relation(posAndPos, negAndPos, posAndNeg, negAndNeg)

  /**
   * If this is `A r B` then return `(!A) r B`.
   */
  def unary_!(): Relation = new Relation(negAndPos, negAndNeg, posAndPos, posAndNeg)

  /**
   * If this is `A r B` and that is `B r C` then
   * this returns `A r (B&C)`.
   */
  def &(that: Relation): Relation = new Relation(
    posAndPos & that.posAndPos,
    posAndNeg | that.posAndNeg,
    negAndPos & that.negAndPos,
    negAndNeg | that.negAndNeg
  )

  /**
   * If this is `A r B` and that is `B r C` then
   * this returns `A r (B|C)`.
   */
  def |(that: Relation): Relation = new Relation(
    posAndPos | that.posAndPos,
    posAndNeg | that.posAndNeg,
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

  override def toString: String = s"Relation(+&+: $posAndPos, +&-: $posAndNeg, -&+: $negAndPos, -&-: $negAndNeg)"
}

object Relation {
  val Same = new Relation(TUnknown, TFalse, TFalse, TUnknown)
  val Complement = new Relation(TFalse, TFalse, TFalse, TTrue)
  val Unknown = new Relation(TUnknown, TUnknown, TUnknown, TUnknown)
}