package nz.rd.whiley

import org.scalatest._

class TypeSpec extends FreeSpec with Matchers {

  "Graph" - {
    "should handle the 'int' type" in {
      val g = Graph.fromTree(Tree.Int)
      g(0) should be (Graph.Node.Int)
    }
    "should handle the '!int' type" in {
      val g = Graph.fromTree(Tree.Negation(Tree.Int))
      g(0) should be (Graph.Node.Negation(1))
      g(1) should be (Graph.Node.Int)
    }
    "should handle the 'int|!int' type" in {
      val g = Graph.fromTree(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int))))
      g(0) should be (Graph.Node.Union(List(1, 2)))
      g(1) should be (Graph.Node.Int)
      g(2) should be (Graph.Node.Negation(3))
      g(3) should be (Graph.Node.Int)
    }
    "should handle the '{int x, int y}' type" in {
      val g = Graph.fromTree(Tree.Record(List(("x", Tree.Int), ("y", Tree.Int))))
      g(0) should be (Graph.Node.Record(List(("x", 1), ("y", 2))))
      g(1) should be (Graph.Node.Int)
      g(2) should be (Graph.Node.Int)
    }
  }

}
