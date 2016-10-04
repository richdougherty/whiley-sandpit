package nz.rd.whiley

import nz.rd.whiley.Shrub.Ref
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class ShrubDNFSpec extends FreeSpec with Matchers with Inside with PropertyChecks {

  import ShrubDNF._

  val ref1 = Shrub.Ref.empty()
  val ref2 = Shrub.Ref.empty()
  val ref3 = Shrub.Ref.empty()

  "ShrubDNF.Term" - {
    "should have expected relationships" in {
      Term.Int.relate(Term.Int).equal should be(TTrue)
      Term.Int.relate(Term.Null).equal should be(TFalse)
      Term.Int.relate(Term.Product(Nil)).equal should be(TFalse)
      Term.Int.relate(Term.Product(List(ref1))).equal should be(TFalse)
      Term.Int.relate(Term.Product(List(ref2))).equal should be(TFalse)

      Term.Null.relate(Term.Int).equal should be(TFalse)
      Term.Null.relate(Term.Null).equal should be(TTrue)
      Term.Null.relate(Term.Product(Nil)).equal should be(TFalse)
      Term.Null.relate(Term.Product(List(ref1))).equal should be(TFalse)
      Term.Null.relate(Term.Product(List(ref2))).equal should be(TFalse)

      Term.Product(Nil).relate(Term.Int).equal should be(TFalse)
      Term.Product(Nil).relate(Term.Null).equal should be(TFalse)
      Term.Product(Nil).relate(Term.Product(Nil)).equal should be(TTrue)
      Term.Product(Nil).relate(Term.Product(List(ref1))).equal should be(TFalse)
      Term.Product(Nil).relate(Term.Product(List(ref2))).equal should be(TFalse)

      Term.Product(List(ref1)).relate(Term.Int).equal should be(TFalse)
      Term.Product(List(ref1)).relate(Term.Null).equal should be(TFalse)
      Term.Product(List(ref1)).relate(Term.Product(Nil)).equal should be(TFalse)
      Term.Product(List(ref1)).relate(Term.Product(List(ref1))).equal should be(TTrue)
      Term.Product(List(ref1)).relate(Term.Product(List(ref2))).equal should be(TUnknown)
    }
  }
  "ShrubDNF.Neg" - {
    "should handle negation" in {
      Neg(Term.Int) should be(Neg(true, Term.Int))
      !Neg(Term.Int) should be(Neg(false, Term.Int))
      !(!Neg(Term.Int)) should be(Neg(true, Term.Int))
    }
    "should relate !int and !<> properly" in {
      val rel = Neg(false, Term.Int).relate(Neg(false, Term.Product(Nil)))
      rel.equal should be(TFalse)
      rel.disjoint should be(TFalse)
      rel.posAndPos should be(TTrue)
      rel.posAndNeg should be(TTrue)
      rel.negAndPos should be(TTrue)
      rel.posAndPos should be(TTrue)
    }
  }

  "ShrubDNF.Conj" - {
    "should have correct truth table for ! operator" in {
      !Conj.True should be(Disj.False)
      !Conj.False should be(Disj.True)
    }
    "should have correct truth table for & operator" in {
      (Conj.True & Conj.True) should be(Conj.True)
      (Conj.True & Conj.False) should be(Conj.False)
      (Conj.False & Conj.True) should be(Conj.False)
      (Conj.False & Conj.False) should be(Conj.False)
    }
    "should have correct truth table for | operator" in {
      (Conj.True | Conj.True) should be(Disj.True)
      (Conj.True | Conj.False) should be(Disj.True)
      (Conj.False | Conj.True) should be(Disj.True)
      (Conj.False | Conj.False) should be(Disj.False)
    }
    "should relate !int and !<> properly" in {
      val rel = Conj.Negs(List(Neg(false, Term.Int))).relate(Conj.Negs(List(Neg(false, Term.Product(Nil)))))
      rel.equal should be(TFalse)
      rel.disjoint should be(TFalse)
      rel.posAndPos should be(TTrue)
      rel.posAndNeg should be(TTrue)
      rel.negAndPos should be(TTrue)
      rel.posAndPos should be(TTrue)
    }
    "!null --> !null" in {
      !Conj(Term.Null) should be(Disj(Conj(!Term.Null)))
    }
    "int&null --> false" in {
      Conj(Term.Int) & Conj(Term.Null) should be(Conj.False)
    }
    "int&!null --> int" in {
      Conj(Term.Int) & !Conj(Term.Null) should be(Disj(Term.Int))
    }
    "<1>&<2> --> <1&2>" in {
      val x: Ref = Ref(Shrub.Int)
      val y: Ref = Ref(Shrub.Null)
      val xAndY = (Conj(Term.Product(List(x))) & Conj(Term.Product(List(y))))
      inside(xAndY) {
        case Conj.Negs(List(Neg(true, Term.Product(List(z))))) => ()
          inside(z.get) {
            case Shrub.Intersection(intersected) =>
              intersected should contain only (Shrub.Int, Shrub.Null)
          }
      }
    }
    "!<1>&<2> --> !<1>&<2>" in {
      val x: Ref = Ref(Shrub.Int)
      val y: Ref = Ref(Shrub.Null)
      val xAndY = ((!Conj(Term.Product(List(x)))) & Conj(Term.Product(List(y))))
      inside(xAndY) {
        case Disj.Conjs(List(Conj.Negs(List(Neg(true, Term.Product(List(z))))))) => ()
          inside(z.get) {
            case Shrub.Intersection(intersected) =>
              intersected should contain only (Shrub.Negation(Shrub.Int), Shrub.Null)
          }
      }
    }
    "<1>&<2,3> --> false" in {
      (Conj(Term.Product(List(ref1))) & Conj(Term.Product(List(ref2,ref3)))) should be(Conj.False)
    }
    "!int&!<> --> !int&!<>" in {
      (Conj.Negs(List(Neg(false, Term.Int))) & Conj.Negs(List(Neg(false, Term.Product(Nil))))) should be(Conj.Negs(List(
        Neg(false, Term.Int),
        Neg(false, Term.Product(Nil))
      )))
    }
  }

  "ShrubDNF.Disj" - {
    "should have correct truth table for ! operator" in {
      !Disj.True should be(Disj.False)
      !Disj.False should be(Disj.True)
    }
    "should have correct truth table for & operator" in {
      (Disj.True & Disj.True) should be(Disj.True)
      (Disj.True & Disj.False) should be(Disj.False)
      (Disj.False & Disj.True) should be(Disj.False)
      (Disj.False & Disj.False) should be(Disj.False)
    }
    "should have correct truth table for | operator" in {
      (Disj.True | Disj.True) should be(Disj.True)
      (Disj.True | Disj.False) should be(Disj.True)
      (Disj.False | Disj.True) should be(Disj.True)
      (Disj.False | Disj.False) should be(Disj.False)
    }
    "should convert !(int|<>) to !int&!<>" in {
      !(Disj(Term.Int) | Term.Product(Nil)) should be(Disj.Conjs(List(
        Conj.Negs(List(
          Neg(false, Term.Int),
          Neg(false, Term.Product(Nil))
        ))
      )))
    }
    "should convert !(!int&!<>) to int|<>" in {
      !(Disj(!Term.Int) & !Term.Product(Nil)) should be(Disj.fromTerms(List(Term.Int, Term.Product(Nil))))
    }
    "should keep int|<> the same" in {
      (Disj(Term.Int) | Term.Product(Nil)) should be(Disj.Conjs(List(
        Conj.Negs(List(Neg(true, Term.Int))),
        Conj.Negs(List(Neg(true, Term.Product(Nil)))))))
    }
    "should keep !int&!<> the same" in {
      (Disj(!Term.Int) & !Term.Product(Nil)) should be(Disj.Conjs(List(Conj.Negs(List(Neg(false, Term.Int), Neg(false, Term.Product(Nil)))))))
    }
    "should convert int&!<> to int" in {
      (Disj(Term.Int) & !Term.Product(Nil)) should be(Disj(Term.Int))
    }
    "should convert !<>&int to int" in {
      (Disj(!Term.Product(Nil)) & Term.Int) should be(Disj(Term.Int))
    }
    "should convert !int&<> to <>" in {
      (Disj(!Term.Int) & Term.Product(Nil)) should be(Disj(Term.Product(Nil)))
    }
    "should convert <>&!int to <>" in {
      (Disj(Term.Product(Nil)) & !Term.Int) should be(Disj(Term.Product(Nil)))
    }
    "should convert int&<> to false" in {
      (Disj(Term.Int) & Term.Product(Nil)) should be(Disj.False)
    }
    "should convert <>&int to false" in {
      (Disj(Term.Product(Nil)) & Term.Int) should be(Disj.False)
    }
    "should convert int|<1>|!<2> to <1>|!<2>" in {
      (Disj(Term.Int) | Term.Product(List(ref1)) | !Term.Product(List(ref2))) should be(Disj.Conjs(List(
        Conj.Negs(List(Neg(true, Term.Product(List(ref1))))),
        Conj.Negs(List(Neg(false, Term.Product(List(ref2)))))
      )))
    }
    "should convert int|!int to any" in {
      (Disj(Term.Int) | !Term.Int) should be(Disj.True)
    }
  }

  "ShrubberyDNF.dnf" - {
    "should convert any to true" in {
      dnf(Shrub.Any) should be(Disj.True)
    }
    "should convert void to false" in {
      dnf(Shrub.Void) should be(Disj.False)
    }
    "should convert int to int" in {
      dnf(Shrub.Int) should be(Disj(Term.Int))
    }
    "should convert null to null" in {
      dnf(Shrub.Null) should be(Disj(Term.Null))
    }
    "should convert null&!null to false" in {
      val d = dnf(Shrub.Intersection(List(Shrub.Null, Shrub.Negation(Shrub.Null))))
      d should be(Disj(Term.Null) & !Term.Null)
      val s = d.toShrub
      s should be(Shrub.Void)
    }
    "should convert !(int|<>) to !int&!<>" in {
      val d = dnf(Shrub.Negation(Shrub.Union(List(Shrub.Int, Shrub.Product(Nil)))))
      d.toShrub should be(Shrub.Intersection(List(Shrub.Negation(Shrub.Int), Shrub.Negation(Shrub.Product(Nil)))))
    }

  }

}
