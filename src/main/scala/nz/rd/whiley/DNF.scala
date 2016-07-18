package nz.rd.whiley

object DNF {

//  object Implicits {
//    implicit def ternaryToConj(ternary: Ternary): Conj = Conj(ternary)
//    implicit def termToConj(term: Term): Conj = Conj(term)
//    implicit def ternaryToDisj(ternary: Ternary): Disj = Disj(Conj(ternary))
//    implicit def termToDisj(term: Term): Disj = Disj(Conj(term))
//    implicit def conjToDisj(conj: Conj): Disj = Disj(conj)
//  }

  private var depth = 0
  private def log(msg: String): Unit = {
    for (_ <- 0 until depth) { print("  ") }
    println(msg)
  }
  private def logCall[A](msg: String)(call: => A): A = {
    log(s"$msg ...")
    depth += 1
    val r = call
    depth -= 1
    log(s"$msg --> $r")
    r
  }

  class Disj(val constant: Ternary, val conjs: Set[Conj]) {
    require(constant != TTrue || conjs.isEmpty)

    def max: Ternary = if (conjs.isEmpty) constant else TTrue

    def |(ternary: Ternary): Disj = this | (Conj(ternary))
    def |(term: Term): Disj = this | (Conj.Empty & term)
    def |(conj: Conj): Disj = {
      logCall(s"$this | $conj") {
        if (constant == TTrue || conj.constant == TFalse) {
          log(s"Ignoring $conj because this's constant is true or conj's constant is false")
          this
        } else if (conj.terms.isEmpty) {
          log(s"Ignoring $conj's because this's constant is true or conj's constant is false")
          new Disj(constant | conj.constant, this.conjs)
        } else if (conjs.exists(c => c.constant == (c.constant | conj.constant) && c.terms.subsetOf(conj.terms))) {
          log(s"Conj $conj is redundant with $this")
          this
        } else {
          log(s"Adding new conj $conj")
          new Disj(constant, conjs + conj)
        }
      }
    }
    def |(that: Disj): Disj = {
      logCall(s"$this | $that") {
        that.conjs.foldLeft(new Disj(constant | that.constant, conjs)) { case (d, c) => d | c }
      }
    }

    def &(ternary: Ternary): Disj = this & Conj(ternary)
    def &(term: Term): Disj = this & Conj(term)
    def &(conj: Conj): Disj = {
      logCall(s"$this & $conj") {
        this.conjs.foldLeft(Disj.Empty | (conj & constant)) { case (d, c) => d | (conj & c) }
      }
    }
    def &(that: Disj): Disj = {
      logCall(s"$this & $that") {
        this.conjs.foldLeft(Disj.Empty | (Conj(constant) & that)) { case (d, c) => d | (c & that) }
      }
    }

    def unary_!(): Disj = {
      logCall(s"!$this") {
        conjs.foldLeft(Disj(!constant)) { (d, c) => d & !c }
      }
    }

    def possibleValues: Set[Ternary] = {
      val start: Set[Ternary] = constant match {
        case TTrue => Set(TTrue)
        case TUnknown => Set(TTrue, TUnknown)
        case TFalse => Set(TTrue, TUnknown, TFalse)
      }
      conjs.foldLeft(start) {
        case (currSet, c) =>
          for {
            a <- currSet
            b <- c.possibleValues
          } yield a | b
      }
    }
    override def equals(other: Any): Boolean = other match {
      case that: Disj => constant == that.constant && conjs == that.conjs
      case _ => false
    }
    override def hashCode(): Int = constant.hashCode ^ conjs.hashCode
    override def toString: String = {
      val constStr = if (constant == TFalse) { '[' + constant.toString + ']' } else { constant.toString }
      "disj(" + (constStr +: conjs.toSeq).mkString(" | ") + ")"
    }
  }

  object Disj {
    def Empty: Disj = False
    val True: Disj = new Disj(TTrue, Set.empty)
    val Unknown: Disj = new Disj(TUnknown, Set.empty)
    val False: Disj = new Disj(TFalse, Set.empty)
    def apply(ternary: Ternary): Disj = ternary match {
      case TTrue => True
      case TUnknown => Unknown
      case TFalse => False
    }
    def apply(term: Term): Disj = apply(Conj(term))
    def apply(conjs: Conj*): Disj = conjs.foldLeft(Empty) { case (d, c) => d | c }
  }

  class Conj(val constant: Ternary, val terms: Set[Term]) {
    require(constant != TFalse || terms.isEmpty)

    def min: Ternary = if (terms.isEmpty) constant else TFalse

    def &(ternary: Ternary): Conj = {
      if (ternary == TTrue) {
        this
      } else if (ternary == TFalse || constant == TFalse) {
        Conj.False
      } else {
        assert(ternary == TUnknown)
        new Conj(TUnknown, terms)
      }
    }
    def &(term: Term): Conj = {
      if (constant == TFalse) {
        // Already false
        Conj.False
      } else if (terms.exists(t => t.pos != term.pos && t.absVal == term.absVal && t.kind == term.kind)) {
        // Equality and inequality contradicts
        Conj.False
      } else if (term.pos && terms.exists(t => t.pos && t.absVal == term.absVal && t.kind != term.kind)) {
        // Kind contradicts
        Conj.False
      } else if (!term.pos && terms.exists(t => t.pos && t.absVal == term.absVal && t.kind != term.kind)) {
        // Redundant to add inequality when already have equality
        this
      } else if (terms.contains(term)) {
        // Redundant
        this
      } else {
        new Conj(constant, terms + term) //
      }
    }
    def &(that: Conj): Conj = {
      logCall(s"$this & $that") {
        that.terms.foldLeft(new Conj(this.constant & that.constant, this.terms)) { case (c, t) => c & t }
      }
    }
    def &(disj: Disj): Disj = {
      logCall(s"$this & $disj") {
        disj & this
      }
    }

    def |(disj: Disj): Disj = Disj(this) | disj
    def |(conj: Conj): Disj = this | Disj(conj)
    def |(ternary: Ternary): Disj = this | Disj(Conj(ternary))
    def |(that: Term): Disj = this | Disj(Conj(that))

    def unary_!(): Disj = {
      logCall(s"!$this") {
        terms.foldLeft(Disj(!constant)) { case (d, t) => d | !t }
      }
    }
    def possibleValues: Set[Ternary] = {
      val start: Set[Ternary] = constant match {
        case TTrue => Set(TTrue, TUnknown, TFalse)
        case TUnknown => Set(TUnknown, TFalse)
        case TFalse => Set(TFalse)
      }
//      terms.foldLeft(start) {
//        case (currSet, term) =>
//          for {
//            a <- currSet
//            b <- term.possibleValues
//          } yield a & b
//      }
      // Optimization for above loop since terms can't evaluate to TUnknown at the current time
      if (terms.isEmpty) { start } else { start - TUnknown }
    }

    override def equals(other: Any): Boolean = other match {
      case that: Conj => constant == that.constant && terms == that.terms
      case _ => false
    }
    override def hashCode(): Int = constant.hashCode ^ terms.hashCode
    override def toString: String = {
      val constStr = if (constant == TTrue) { '[' + constant.toString + ']' } else { constant.toString }
      "conj(" + (constStr +: terms.toSeq).mkString(" & ") + ")"
    }
  }

  object Conj {
    val True: Conj = new Conj(TTrue, Set.empty)
    val Unknown: Conj = new Conj(TUnknown, Set.empty)
    val False: Conj = new Conj(TFalse, Set.empty)
    def Empty: Conj = True
    def apply(ternary: Ternary): Conj = ternary match {
      case TTrue => True
      case TUnknown => Unknown
      case TFalse => False
    }
    def apply(terms: Term*): Conj = terms.foldLeft(Conj.Empty) { case (c, t) => c & t }
  }

  final case class Term(pos: Boolean, absVal: AbsVal, kind: Kind) {
    def unary_!(): Term = Term(!pos, absVal, kind)
    def &(ternary: Ternary): Conj = this & Conj(ternary)
    def &(that: Term): Conj = this & Conj(that)
    def &(conj: Conj): Conj = Conj(this) & conj
    def |(disj: Disj): Disj = Disj(Conj(this)) | disj
    def |(conj: Conj): Disj = this | Disj(conj)
    def |(ternary: Ternary): Disj = this | Disj(Conj(ternary))
    def |(that: Term): Disj = this | Disj(Conj(that))
    def possibleValues: Set[Ternary] = Set(TTrue, TFalse)
    override def toString: String = {
      (if (pos) "" else "!") + absVal + ":" + kind
    }
  }
  object Term {
    def isKind(absVal: AbsVal, kind: Kind): Term = Term(true, absVal, kind)
    def isNotKind(absVal: AbsVal, kind: Kind): Term = !isKind(absVal, kind)
  }

  sealed trait AbsVal
  final case object RootVal extends AbsVal {
    override def toString: String = "root"
    def apply(index: Int): ProductVal = ProductVal(this, index)
  }
  final case class ProductVal(parent: AbsVal, index: Int) extends AbsVal {
    override def toString: String = s"$parent[$index]"
  }

  sealed trait Kind
  object Kind {
    case object Null extends Kind
    case object Int extends Kind
    case class Product(size: Int) extends Kind
  }

}