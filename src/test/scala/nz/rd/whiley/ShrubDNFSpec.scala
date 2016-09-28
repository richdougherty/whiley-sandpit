package nz.rd.whiley

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class ShrubDNFSpec extends FreeSpec with Matchers with Inside with PropertyChecks {

  import ShrubDNF._

  "ShrubDNF.Term" - {
    "should have expected relationshibs" in {
      Term.Int.relationship(Term.Int) should be(Term.Relationship.Same)
      Term.Int.relationship(Term.Null) should be(Term.Relationship.Different)
      Term.Int.relationship(Term.Product(Nil)) should be(Term.Relationship.Different)
      Term.Int.relationship(Term.Product(List(1))) should be(Term.Relationship.Different)
      Term.Int.relationship(Term.Product(List(2))) should be(Term.Relationship.Different)

      Term.Null.relationship(Term.Int) should be(Term.Relationship.Different)
      Term.Null.relationship(Term.Null) should be(Term.Relationship.Same)
      Term.Null.relationship(Term.Product(Nil)) should be(Term.Relationship.Different)
      Term.Null.relationship(Term.Product(List(1))) should be(Term.Relationship.Different)
      Term.Null.relationship(Term.Product(List(2))) should be(Term.Relationship.Different)

      Term.Product(Nil).relationship(Term.Int) should be(Term.Relationship.Different)
      Term.Product(Nil).relationship(Term.Null) should be(Term.Relationship.Different)
      Term.Product(Nil).relationship(Term.Product(Nil)) should be(Term.Relationship.Same)
      Term.Product(Nil).relationship(Term.Product(List(1))) should be(Term.Relationship.Different)
      Term.Product(Nil).relationship(Term.Product(List(2))) should be(Term.Relationship.Different)

      Term.Product(List(1)).relationship(Term.Int) should be(Term.Relationship.Different)
      Term.Product(List(1)).relationship(Term.Null) should be(Term.Relationship.Different)
      Term.Product(List(1)).relationship(Term.Product(Nil)) should be(Term.Relationship.Different)
      Term.Product(List(1)).relationship(Term.Product(List(1))) should be(Term.Relationship.Same)
      Term.Product(List(1)).relationship(Term.Product(List(2))) should be(Term.Relationship.Unknown)
    }
  }
  "ShrubDNF.Neg" - {
    "should handle negation" in {
      Neg(Term.Int) should be(Neg(true, Term.Int))
      !Neg(Term.Int) should be(Neg(false, Term.Int))
      !(!Neg(Term.Int)) should be(Neg(true, Term.Int))
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
    "!null --> !null" in {
      !Conj(Term.Null) should be(Disj(Conj(!Term.Null)))
    }
    "int&null --> false" in {
      Conj(Term.Int) & Conj(Term.Null) should be(Conj.False)
    }
    "int&!null --> int" in {
      Conj(Term.Int) & !Conj(Term.Null) should be(Disj(Term.Int))
    }
    "<1>&<2> --> <1>&<2>" in {
      (Conj(Term.Product(List(1))) & Conj(Term.Product(List(2)))) should be((Conj(Term.Product(List(1))) & Conj(Term.Product(List(2)))))
    }
    "!<1>&<2> --> !<1>&<2>" in {
      (!Conj(Term.Product(List(1))) & Conj(Term.Product(List(2)))) should be(
        Disj(Conj.Negs(List(
          Neg(false, Term.Product(List(1))),
          Neg(true, Term.Product(List(2)))
        )))
      )
    }
    "<1>&<2,3> --> false" in {
      (Conj(Term.Product(List(1))) & Conj(Term.Product(List(2,3)))) should be(Conj.False)
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
      (Disj(Term.Int) | Term.Product(List(1)) | !Term.Product(List(2))) should be(Disj.Conjs(List(
        Conj.Negs(List(Neg(true, Term.Product(List(1))))),
        Conj.Negs(List(Neg(false, Term.Product(List(2)))))
      )))
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
