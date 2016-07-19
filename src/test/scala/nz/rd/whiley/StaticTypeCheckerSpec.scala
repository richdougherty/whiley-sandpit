package nz.rd.whiley

import org.scalatest._
import scala.collection.mutable

class StaticTypeCheckerSpec extends FreeSpec with Matchers {

  def check(t: Tree): Set[DNF.Disj] = check(Graph.fromTree(t))
  def check(g: Graph): Set[DNF.Disj] = {
    StaticTypeChecker.check(g).toSet
  }

  "StaticTypeChecker" - {

    "should typecheck null as root:null" in {
      check(Tree.Null) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Null))))
    }
    "should typecheck !null as !root:null" in {
      check(Tree.Negation(Tree.Null)) should be(Set(DNF.Disj(DNF.Term.isNotKind(DNF.RootVal, DNF.Kind.Null))))
    }
    "should typecheck int as root:int" in {
      check(Tree.Int) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))))
    }
    "should typecheck !int as !root:int" in {
      check(Tree.Negation(Tree.Int)) should be(Set(DNF.Disj(DNF.Term.isNotKind(DNF.RootVal, DNF.Kind.Int))))
    }
    "should typecheck !!int as root:int" in {
      check(Tree.Negation(Tree.Negation(Tree.Int))) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))))
    }
    "should typecheck void as false" in {
      check(Tree.Void) should be(Set(DNF.Disj.False))
    }
    "should typecheck any as true" in {
      check(Tree.Any) should be(Set(DNF.Disj.True))
    }
    "should typecheck !void as true" in {
      check(Tree.Negation(Tree.Void)) should be(Set(DNF.Disj.True))
    }
    "should typecheck !any as false" in {
      check(Tree.Negation(Tree.Any)) should be(Set(DNF.Disj.False))
    }
    "should typecheck void|int as root:int" in {
      check(Tree.Union(List(Tree.Void, Tree.Int))) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))))
    }
    "should typecheck null|int as root:null | root:int" in {
      check(Tree.Union(List(Tree.Null, Tree.Int))) should be(Set(
        DNF.Disj(
          DNF.Conj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Null)),
          DNF.Conj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))
        )
      ))
    }
    "should typecheck µX.!X as unknown" in {
      check(Tree.Recursive("X", Tree.Negation(Tree.Variable("X")))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should typecheck !(µX.!X) as unknown" in {
      check(Tree.Negation(Tree.Recursive("X", Tree.Negation(Tree.Variable("X"))))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should typecheck µX.(X|) as unknown" in {
      check(Tree.Recursive("X", Tree.Union(List(Tree.Variable("X"))))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should typecheck !µX.(X|) as unknown" in {
      check(Tree.Negation(Tree.Recursive("X", Tree.Union(List(Tree.Variable("X")))))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should typecheck µX.!(X|) as unknown" in {
      check(Tree.Recursive("X", Tree.Negation(Tree.Union(List(Tree.Variable("X")))))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should typecheck µX.X|int as unknown | root:int" in {
      check(Tree.Recursive("X", Tree.Union(List(Tree.Variable("X"), Tree.Int)))) should be(Set(
        DNF.Disj(TUnknown) | DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))
      ))
    }
    "should typecheck <int> as root:product(1) & root[0]:int" in {
      check(Tree.Product(List(Tree.Int))) should be(Set(
        DNF.Disj(DNF.Conj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Product(1)), DNF.Term.isKind(DNF.ProductVal(DNF.RootVal, 0), DNF.Kind.Int)))
      ))
    }
  }

}
