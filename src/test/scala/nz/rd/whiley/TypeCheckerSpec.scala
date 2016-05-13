package nz.rd.whiley

import org.scalatest._
import scala.collection.mutable

class TypeCheckerSpec extends FreeSpec with Matchers {

  "TypeChecker" - {

    "should typecheck 1 as int" in {
      TypeChecker.check(Value.Int(1), Tree.Int) should be(true)
    }
    "should not typecheck 1 as void" in {
      TypeChecker.check(Value.Int(1), Tree.Void) should be(false)
    }
    "should typecheck 1 as void|int" in {
      TypeChecker.check(Value.Int(1), Tree.Union(List(Tree.Void, Tree.Int))) should be(true)
    }
    // "should typecheck {x:1} as {x:int}" in {
    //   TypeChecker.check(Value.Record(List(("x", Value.Int(1)))), Tree.Record(List(("x", Tree.Int)))) should be(true)
    // }

  }

}
