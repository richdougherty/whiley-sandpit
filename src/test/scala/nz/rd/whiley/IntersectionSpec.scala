package nz.rd.whiley

import org.scalatest._
import scala.collection.mutable

class IntersectionSpec extends FreeSpec with Matchers {

  "Intersections" - {

    "of the root with itself" - {

      def runIntersection(t: Tree): (Tree, Intersections) = {
        val alg = new IntersectionAlgorithm(Graph.fromTree(t), mutable.Map.empty)
        alg.calculate()
        val tree = alg.g.toTree
        val ints = alg.get(alg.g.root, alg.g.root)
        (tree, ints)
      }

      "should handle the 'any' type" in {
        val (tree, ints) = runIntersection(Tree.Any)
        tree should be (Tree.Any)
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(Empty))
      }
      "should handle the 'void' type" in {
        val (tree, ints) = runIntersection(Tree.Void)
        tree should be(Tree.Void)
        ints.pp should be(Some(Empty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the 'int' type" in {
        val (tree, ints) = runIntersection(Tree.Int)
        tree should be(Tree.Int)
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }

    }

    "of unions" - {

      "should handle the 'int|!int' type" in {
        val alg = new IntersectionAlgorithm(Graph.fromTree(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int)))), mutable.Map.empty)
        alg.calculate()
      }

    }

  }

}
