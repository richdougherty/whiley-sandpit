package nz.rd.whiley

import org.scalatest._
import scala.collection.mutable

class StaticTypeCheckerSpec extends FreeSpec with Matchers {

  def check(t: Tree): Set[DNF.Disj] = check(Graph.fromTree(t))
  def check(g: Graph): Set[DNF.Disj] = {
    StaticTypeChecker.check(g).toSet
  }

  "StaticTypeChecker" - {

    "should typecheck null" in {
      check(Tree.Null) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Null))))
    }
    "should typecheck int" in {
      check(Tree.Int) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))))
    }
    "should typecheck void" in {
      check(Tree.Void) should be(Set(DNF.Disj.False))
    }
    "should typecheck any" in {
      check(Tree.Any) should be(Set(DNF.Disj.True))
    }
    "should typecheck void|int" in {
      check(Tree.Union(List(Tree.Void, Tree.Int))) should be(Set(DNF.Disj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))))
    }
    "should typecheck null|int" in {
      check(Tree.Union(List(Tree.Null, Tree.Int))) should be(Set(
        DNF.Disj(
          DNF.Conj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Null)),
          DNF.Conj(DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int))
        )
      ))
    }
  }

}
