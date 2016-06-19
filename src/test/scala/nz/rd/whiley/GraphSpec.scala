package nz.rd.whiley

import org.scalatest._

class TypeSpec extends FreeSpec with Matchers {

  def roundTrip(t: Tree): Unit = {
    val g = Graph.fromTree(t)
    val t2 = g.toTree
    t2 should be (t)
  }

  "Graph" - {
    "should handle the 'any' type" in {
      roundTrip(Tree.Any)
    }
    "should handle the 'void' type" in {
      roundTrip(Tree.Void)
    }
    "should handle the 'null' type" in {
      roundTrip(Tree.Null)
    }
    "should handle the 'int' type" in {
      roundTrip(Tree.Int)
    }
    "should handle the '!int' type" in {
      roundTrip(Tree.Negation(Tree.Int))
    }
    "should handle the 'int|!int' type" in {
      roundTrip(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int))))
    }
    "should handle the '<int, int>' type" in {
      roundTrip(Tree.Product(List(Tree.Int, Tree.Int)))
    }
    "should handle the 'µX.X|int' type" in {
      roundTrip(Tree.Recursive("X0", Tree.Union(List(Tree.Variable("X0"), Tree.Int))))
    }
  }

}
