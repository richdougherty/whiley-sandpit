package nz.rd.whiley

import nz.rd.whiley.Shrub.Ref

import scala.annotation.tailrec

object ShrubDNF {

  sealed trait Disj {
    def unary_!(): Disj
    def |(that: Conj.Negs): Disj
    def &(that: Neg): Disj
    def toShrub: Shrub
    def relate(that: Disj): Relation

    final def |(b: Boolean): Disj = this | Disj(b)
    final def |(that: Term): Disj = this | Disj(that)
    final def |(that: Neg): Disj = this | Conj(that)
    final def |(that: Conj): Disj = that match {
      case Conj.True => Disj.True
      case Conj.False => this
      case c: Conj.Negs => this | c
    }
    final def |(that: Disj): Disj = that match {
      case Disj.True => Disj.True
      case Disj.False => this
      case Disj.Conjs(conjs) => conjs.foldLeft(this)(_ | _)
    }
    final def &(b: Boolean): Disj = this & Disj(b)
    final def &(that: Term): Disj = this & Disj(that)
    final def &(that: Conj): Disj = that match {
      case Conj.True => this
      case Conj.False => Disj.False
      case Conj.Negs(negs) => negs.foldLeft(this)(_ & _)
    }
    final def &(that: Disj): Disj = that match {
      case Disj.True => this
      case Disj.False => Disj.False
      case Disj.Conjs(conjs) => Disj.fromDisjs(conjs.map(this & _))
    }
  }

  object Disj {
    case object True extends Disj {
      override def unary_!(): Disj = False
      override def |(that: Conj.Negs): Disj = True
      override def &(that: Neg): Disj = Disj(that)
      def relate(that: Disj): Relation = that match {
        case Disj.True => new Relation(TTrue, TFalse, TFalse, TFalse)
        case Disj.False => new Relation(TFalse, TTrue, TFalse, TFalse)
        case _ => new Relation(TTrue, TTrue, TFalse, TFalse)
      }
      override def toString: String = "Disj.True"
      override def toShrub: Shrub = Shrub.Any
    }
    case object False extends Disj {
      override def unary_!(): Disj = True
      override def |(that: Conj.Negs): Disj = Disj.Conjs(that::Nil)
      override def &(that: Neg): Disj = False
      override def relate(that: Disj): Relation = that match {
        case Disj.True => new Relation(TFalse, TTrue, TFalse, TFalse)
        case Disj.False => new Relation(TFalse, TFalse, TFalse, TTrue)
        case _ => new Relation(TFalse, TFalse, TTrue, TTrue)
      }
      override def toShrub: Shrub = Shrub.Void
      override def toString: String = "Disj.False"
    }
    case class Conjs(conjs: List[Conj.Negs]) extends Disj {
      require(!conjs.isEmpty)
      override def unary_!(): Disj = {
        val x: List[Disj] = conjs.map {
          case Conj.Negs(negs) =>
            val negNegs: List[Neg] = negs.map(!_)
            val disjFromConj: Disj = Disj.fromNegs(negNegs)
            disjFromConj
        }
        val res = x.foldLeft[Disj](Disj.True)(_ & _)
        res
      }
      override def |(that: Conj.Negs): Disj = {
        @tailrec
        def loop(acc: List[Conj.Negs], remaining: List[Conj.Negs]): Disj = remaining match {
          case head :: tail =>
            val rel = head.relate(that)
            if (rel.posOrNeg.isTrue) {
              this
            } else if (rel.negOrPos.isTrue) {
              loop(acc, tail)
            } else if (rel.posOrPos.isTrue) {
              Disj.True
            } else {
              loop(head :: acc, tail)
            }
          case Nil =>
            Disj.Conjs(that :: acc)
        }
        loop(Nil, conjs)
      }
      override def &(that: Neg): Disj = Disj.fromConjs(conjs.map(_ & that))
      override def relate(that: Disj): Relation = that match {
        case Disj.True => new Relation(TTrue, TFalse, TTrue, TFalse)
        case Disj.False => new Relation(TFalse, TTrue, TFalse, TTrue)
        case Disj.Conjs(otherConjs) =>
          conjs.map { conj =>
            otherConjs.map(conj.relate(_)).reduce(_ | _).flip
          }.reduce(_ | _).flip
      }
      override def equals(that: Any): Boolean = that match {
        case Conjs(otherConjs) => conjs.toSet == otherConjs.toSet
        case _ => false
      }
      override def toString: String = s"Disj.Conjs($conjs)"
      override def toShrub: Shrub = conjs match {
        case neg::Nil => neg.toShrub
        case negs => Shrub.Union(negs.map(_.toShrub))
      }
    }
    def apply(b: Boolean): Disj = if (b) True else False
    def apply(t: Term): Disj = Disj(Neg(t))
    def apply(n: Neg): Disj = Disj(Conj(n))
    def apply(c: Conj): Disj = c match {
      case Conj.True => Disj.True
      case Conj.False => Disj.False
      case c@Conj.Negs(_) => Disj.Conjs(c::Nil)
    }
    def fromTerms(ts: List[Term]): Disj = fromDisjs(ts.map(Disj(_)))
    def fromNegs(ns: List[Neg]): Disj = fromDisjs(ns.map(Disj(_)))
    def fromConjs(cs: List[Conj]): Disj = fromDisjs(cs.map(Disj(_)))
    def fromDisjs(ds: List[Disj]): Disj = ds.foldLeft[Disj](Disj.False)(_ | _)
  }

  sealed trait Conj {
    def &(that: Neg): Conj
    def relate(that: Conj): Relation
    def toShrub: Shrub

    final def unary_!(): Disj = !Disj(this)
    final def &(that: Boolean): Conj = if (that) this else Conj.False
    final def &(that: Term): Conj = this & Neg(that)
    final def &(that: Conj): Conj = that match {
      case Conj.True => this
      case Conj.False => Conj.False
      case Conj.Negs(negs) => negs.foldLeft(this)(_ & _)
    }
    final def &(that: Disj): Disj = Disj(this) & that

    final def |(that: Boolean): Disj = Disj(this) | that
    final def |(that: Term): Disj = Disj(this) | that
    final def |(that: Neg): Disj = Disj(this) | that
    final def |(that: Conj): Disj = Disj(this) | that
    final def |(that: Disj): Disj = Disj(this) | that
  }

  object Conj {
    case object True extends Conj {
      override def &(that: Neg): Conj = Conj(that)
      override def relate(that: Conj): Relation = that match {
        case Conj.True => new Relation(TTrue, TFalse, TFalse, TFalse)
        case Conj.False => new Relation(TFalse, TTrue, TFalse, TFalse)
        case _ => new Relation(TTrue, TTrue, TFalse, TFalse)
      }
      override def toShrub: Shrub = Shrub.Any
      override def toString: String = "Conj.True"
    }
    case object False extends Conj {
      override def &(that: Neg): Conj = Conj.False
      override def relate(that: Conj): Relation = that match {
        case Conj.True => new Relation(TFalse, TTrue, TFalse, TFalse)
        case Conj.False => new Relation(TFalse, TFalse, TFalse, TTrue)
        case _ => new Relation(TFalse, TFalse, TTrue, TTrue)
      }
      override def toShrub: Shrub = Shrub.Void
      override def toString: String = "Conj.False"
    }
    case class Negs(negs: List[Neg]) extends Conj {
      override def &(that: Neg): Conj = {
        @tailrec
        def loop(acc: List[Neg], remaining: List[Neg]): Conj = remaining match {
          case head :: tail =>
            val rel = head.relate(that)
            if (rel.posAndNeg.isFalse) {
              this
            } else if (rel.negAndPos.isFalse) {
              loop(acc, tail)
            } else if (rel.posAndPos.isFalse) {
              Conj.False
            } else {
              (head, that) match {
                case (Neg(aSign, Term.Product(aRefs)), Neg(bSign, Term.Product(bRefs))) =>
                  assert(aRefs.length == bRefs.length)
                  val combined = Neg(true, Term.Product((aRefs zip bRefs).map {
                    case (aChildRef, bChildRef) =>
                      val aChildShrub: Shrub = if (aSign) aChildRef.get else Shrub.Negation(aChildRef.get)
                      val bChildShrub: Shrub = if (bSign) bChildRef.get else Shrub.Negation(bChildRef.get)
                      Ref(Shrub.Intersection(List(aChildShrub, bChildShrub)))
                  }))
                  Conj.Negs(acc++(combined::tail))
                case _ =>
                  loop(head :: acc, tail)
              }
            }
          case Nil =>
            Conj.Negs(that :: acc)
        }
        loop(Nil, negs)
      }
      override def relate(that: Conj): Relation = that match {
        case Conj.True => new Relation(TTrue, TFalse, TTrue, TFalse)
        case Conj.False => new Relation(TFalse, TTrue, TFalse, TTrue)
        case Conj.Negs(otherNegs) =>
          negs.map { neg =>
            otherNegs.map(neg.relate(_)).reduce(_ & _).flip
          }.reduce(_ & _).flip
      }

      override def toShrub: Shrub = negs match {
        case neg::Nil => neg.toShrub
        case negs => Shrub.Intersection(negs.map(_.toShrub))
      }
      override def equals(that: Any): Boolean = that match {
        case Negs(otherNegs) => negs.toSet == otherNegs.toSet
        case _ => false
      }
      override def toString: String = s"Conj.Negs($negs)"
    }

    def apply(b: Boolean): Conj = if (b) True else False
    def apply(t: Term): Conj = Conj(Neg(t))
    def apply(n: Neg): Conj = Conj.Negs(n::Nil)
  }

  case class Neg(sign: Boolean, term: Term) {
    def unary_!(): Neg = Neg(!sign, term)
    def relate(that: Neg): Relation = {
      var termRel = term.relate(that.term)
      if (!sign) termRel = !termRel
      if (!that.sign) termRel = (!(termRel.flip)).flip
      termRel
    }
    def toShrub: Shrub = if (sign) term.toShrub else Shrub.Negation(term.toShrub)
  }

  object Neg {
    sealed trait Relationship
    object Relationship {
      case object Equal extends Relationship
      case object Superset extends Relationship
      case object Subset extends Relationship
      case object Disjoint extends Relationship
      case object Complement extends Relationship
    }

    def apply(t: Term): Neg = Neg(true, t)
  }

  sealed trait Term {
    def unary_!(): Neg = Neg(false, this)
    def relate(that: Term): Relation = (this, that) match {
      // PERF: Cache relations
      case (a, b) if a == b => new Relation(TTrue, TFalse, TFalse, TTrue)
      case (Term.Product(refs1), Term.Product(refs2)) if refs1.length == refs2.length => new Relation(TUnknown, TUnknown, TUnknown, TTrue)
      case _ => new Relation(TFalse, TTrue, TTrue, TTrue)
    }
    def toShrub: Shrub
  }
  object Term {
    case object Int extends Term {
      override def toShrub: Shrub = Shrub.Int
    }
    case object Null extends Term {
      override def toShrub: Shrub = Shrub.Null
    }
    case class Product(refs: List[Shrub.Ref]) extends Term {
      override def toShrub: Shrub = Shrub.Product(refs)
    }

    sealed trait Relationship
    object Relationship {
      case object Same extends Relationship
      case object Different extends Relationship
      case object Unknown extends Relationship
    }
  }

  def dnf(s: Shrub): Disj = s match {
    case Shrub.Any => Disj(true)
    case Shrub.Void => Disj(false)
    case Shrub.Null => Disj(Term.Null)
    case Shrub.Int => Disj(Term.Int)
    case Shrub.Negation(child) => !dnf(child)
    case Shrub.Union(children) => Disj.fromDisjs(children.map(dnf))
    case Shrub.Intersection(children) => children.map(dnf).foldLeft[Disj](Disj.True)(_ & _)
    case Shrub.Product(refs) => Disj(Term.Product(refs))
  }

}
