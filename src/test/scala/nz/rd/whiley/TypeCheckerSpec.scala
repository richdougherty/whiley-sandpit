package nz.rd.whiley

import org.scalatest._
import scala.collection.mutable

class TypeCheckerSpec extends FreeSpec with Matchers {

  "TypeChecker" - {

    "should typecheck null as null" in {
      TypeChecker.check(Value.Null, Tree.Null) should be(true)
    }
    "should typecheck 1 as int" in {
      TypeChecker.check(Value.Int(1), Tree.Int) should be(true)
    }
    "should not typecheck 1 as void" in {
      TypeChecker.check(Value.Int(1), Tree.Void) should be(false)
    }
    "should typecheck 1 as void|int" in {
      TypeChecker.check(Value.Int(1), Tree.Union(List(Tree.Void, Tree.Int))) should be(true)
    }
    "should typecheck 1 as µX.X|int" in {
      TypeChecker.check(Value.Int(1), Tree.Recursive("X", Tree.Union(List(Tree.Variable("X"), Tree.Int)))) should be(true)
    }
    "should not typecheck int as !int" in {
      TypeChecker.check(Value.Int(0), Tree.Negation(Tree.Int)) should be(false)
    }
    "should typecheck null as !int" in {
      TypeChecker.check(Value.Null, Tree.Negation(Tree.Int)) should be(true)
    }
    "should typecheck int as !!int" in {
      TypeChecker.check(Value.Int(-1), Tree.Negation(Tree.Negation(Tree.Int))) should be(true)
    }
    "should not typecheck null as !!int" in {
      TypeChecker.check(Value.Null, Tree.Negation(Tree.Negation(Tree.Int))) should be(false)
    }
    "should typecheck {x:1} as {x:int}" in {
      TypeChecker.check(Value.Record(List(("x", Value.Int(1)))), Tree.Record(List(("x", Tree.Int)))) should be(true)
    }
    "should typecheck {x:1} as !int" in {
      TypeChecker.check(Value.Record(List(("x", Value.Int(1)))), Tree.Negation(Tree.Int)) should be(true)
    }
    "should not typecheck {x:1} as {x:!int}" in {
      TypeChecker.check(Value.Record(List(("x", Value.Int(1)))), Tree.Record(List(("x", Tree.Negation(Tree.Int))))) should be(false)
    }
    "should not typecheck {x:1} as {y:int}" in {
      TypeChecker.check(Value.Record(List(("x", Value.Int(1)))), Tree.Record(List(("y", Tree.Int)))) should be(false)
    }

  }

}
