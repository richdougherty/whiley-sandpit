package nz.rd.whiley

import org.scalatest._
import scala.collection.mutable

class StaticTypeCheckerSpec extends FreeSpec with Matchers {

  def check(t: Tree): Set[DNF.Disj] = check(Graph.fromTree(t))
  def check(g: Graph): Set[DNF.Disj] = {
    StaticTypeChecker.check(g).toSet
  }
  def possibleValues(t: Tree): Set[Ternary] = possibleValues(Graph.fromTree(t))
  def possibleValues(g: Graph): Set[Ternary] = {
    StaticTypeChecker.check(g).toSet.flatMap((d: DNF.Disj) => d.possibleValues)
  }

  "StaticTypeChecker" - {

    "should convert root:null to root:null" in {
      check(Tree.Null) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Null))))
    }
    "should convert root:!null to !root:null" in {
      check(Tree.Negation(Tree.Null)) should be(Set(DNF.Disj(DNF.Term.isNotKind(DNF.RootVal, DNF.Kind.Null))))
    }
    "should convert root:int to root:int" in {
      check(Tree.Int) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))))
    }
    "should typecheck !int to !root:int" in {
      check(Tree.Negation(Tree.Int)) should be(Set(DNF.Disj(DNF.Term.isNotKind(DNF.RootVal, DNF.Kind.Int))))
    }
    "should convert root:!!int to root:int" in {
      check(Tree.Negation(Tree.Negation(Tree.Int))) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))))
    }
    "should convert root:void to false" in {
      check(Tree.Void) should be(Set(DNF.Disj.False))
    }
    "should convert root:any to true" in {
      check(Tree.Any) should be(Set(DNF.Disj.True))
    }
    "should convert root:!void to true" in {
      check(Tree.Negation(Tree.Void)) should be(Set(DNF.Disj.True))
    }
    "should convert root:!any to false" in {
      check(Tree.Negation(Tree.Any)) should be(Set(DNF.Disj.False))
    }
    "should convert root:(any|int) to true" in {
      check(Tree.Union(List(Tree.Any, Tree.Int))) should be(Set(DNF.Disj(TTrue)))
    }
    "should convert root:(void|int) to root:int" in {
      check(Tree.Union(List(Tree.Void, Tree.Int))) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))))
    }
    "should convert root:(null|int) to (root:null | root:int)" in {
      check(Tree.Union(List(Tree.Null, Tree.Int))) should be(Set(
        DNF.Disj(
          DNF.Conj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Null)),
          DNF.Conj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))
        )
      ))
    }
    "should convert root:(any&int) to root:int" in {
      check(Tree.Intersection(List(Tree.Any, Tree.Int))) should be(Set(
        DNF.Disj(TTrue) & DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))
      ))
    }
    "should convert root:(void&int) to false" in {
      check(Tree.Intersection(List(Tree.Void, Tree.Int))) should be(Set(DNF.Disj(TFalse)))
    }
    "should convert root:(null&int) to false" in {
      check(Tree.Intersection(List(Tree.Null, Tree.Int))) should be(Set(DNF.Disj(TFalse)))
    }
    "should convert root:(null&<int>) to false" in {
      check(Tree.Intersection(List(Tree.Null, Tree.Product(List(Tree.Int))))) should be(Set(DNF.Disj(TFalse)))
    }
    "should convert root:(µX.!X) to unknown" in {
      check(Tree.Recursive("X", Tree.Negation(Tree.Variable("X")))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should convert root:!(µX.!X) to unknown" in {
      check(Tree.Negation(Tree.Recursive("X", Tree.Negation(Tree.Variable("X"))))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should convert root:µX.(X|) to unknown" in {
      check(Tree.Recursive("X", Tree.Union(List(Tree.Variable("X"))))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should convert root:(µX.(X&)) to unknown" in {
      check(Tree.Recursive("X", Tree.Intersection(List(Tree.Variable("X"))))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should convert root:(!µX.(X&)) to unknown" in {
      check(Tree.Negation(Tree.Recursive("X", Tree.Intersection(List(Tree.Variable("X")))))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should convert root:(µX.!(X|)) to unknown" in {
      check(Tree.Recursive("X", Tree.Negation(Tree.Union(List(Tree.Variable("X")))))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should convert root:(µX.!(X&)) to unknown" in {
      check(Tree.Recursive("X", Tree.Negation(Tree.Intersection(List(Tree.Variable("X")))))) should be(Set(DNF.Disj(TUnknown)))
    }
    "should convert root:(µX.(X|int)) to (unknown | root:int)" in {
      check(Tree.Recursive("X", Tree.Union(List(Tree.Variable("X"), Tree.Int)))) should be(Set(
        DNF.Disj(TUnknown) | DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))
      ))
    }
    "should convert root:(µX.(X&int)) to (unknown & root:int)" in {
      check(Tree.Recursive("X", Tree.Intersection(List(Tree.Variable("X"), Tree.Int)))) should be(Set(
        DNF.Disj(TUnknown) & DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))
      ))
    }
    "should convert root:<int> to (root:product(1) & root[0]:int)" in {
      check(Tree.Product(List(Tree.Int))) should be(Set(
        DNF.Disj(DNF.Conj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Product(1)), DNF.Term.isKind(DNF.ProductVal(DNF.RootVal, 0), DNF.Kind.Int)))
      ))
    }
    "should convert root:<!int> to (root:product(1) & !root[0]:int)" in {
      check(Tree.Product(List(Tree.Negation(Tree.Int)))) should be(Set(
        DNF.Disj(DNF.Conj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Product(1)), DNF.Term.isNotKind(DNF.ProductVal(DNF.RootVal, 0), DNF.Kind.Int)))
      ))
    }
    "should convert root:(µX.<X>) to false" in {
      check(Tree.Recursive("X", Tree.Product(List(Tree.Variable("X"))))) should be(Set(
        DNF.Disj(TFalse)
      ))
    }
    "superset of possible results for root:(µX.<X>) should be {false}" in {
      possibleValues(Tree.Recursive("X", Tree.Product(List(Tree.Variable("X"))))) should be(Set(TFalse))
    }
    "superset of possible results for root:(µX.<!X>) should be {true, false}" in {
      possibleValues(Tree.Recursive("X", Tree.Product(List(Tree.Negation(Tree.Variable("X")))))) should be(Set(TTrue, TFalse))
    }
    "superset of possible results for root:(µX.(null|<int,X>)) should be {true, false}" in {
      possibleValues(Tree.Recursive("X", Tree.Union(List(Tree.Null, Tree.Product(List(Tree.Int, Tree.Variable("X"))))))) should be(Set(TTrue, TFalse))
    }
    "superset of possible results for root:(µX.(null&<int,X>)) should be {false}" in {
      possibleValues(Tree.Recursive("X", Tree.Intersection(List(Tree.Null, Tree.Product(List(Tree.Int, Tree.Variable("X"))))))) should be(Set(TFalse))
    }
  }

}
