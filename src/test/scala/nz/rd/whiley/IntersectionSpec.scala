package nz.rd.whiley

import org.scalatest._
import scala.collection.mutable

class IntersectionSpec extends FreeSpec with Matchers {

  "Intersections" - {

    "of the root with itself" - {

      def runIntersection(t: Tree): (Tree, Contents, Intersections) = {
        println(s"--- Running intersection on $t ---")
        val alg = IntersectionAlgorithm.forGraph(Graph.fromTree(t))
        alg.calculate()
        println(s"Alg: nodes: ${alg.g.root}, ${alg.g.nodes}, ${alg.contents}, ${alg.intersections}")
        val tree = alg.g.toTree
        val rootConts = alg.getContents(alg.g.root)
        val rootInts = alg.getInts(alg.g.root, alg.g.root)
        println(s"--- Done ---")
        (tree, rootConts, rootInts)
      }

      "should handle the 'any' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Any)
        tree should be (Tree.Any)
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(Empty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(Empty))
      }
      "should handle the 'void' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Void)
        tree should be(Tree.Void)
        conts.p should be(Some(Empty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(Empty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the 'int' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Int)
        tree should be(Tree.Int)
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the '!any' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Negation(Tree.Any))
        tree should be(Tree.Void)
        conts.p should be(Some(Empty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(Empty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the '!void' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Negation(Tree.Void))
        tree should be(Tree.Any)
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(Empty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(Empty))
      }
      "should handle the '!int' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Negation(Tree.Int))
        tree should be(Tree.Negation(Tree.Int))
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the '!!int' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Negation(Tree.Negation(Tree.Int)))
        tree should be(Tree.Int)
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the '|' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Union(Nil))
        tree should be(Tree.Void)
        conts.p should be(Some(Empty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(Empty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the 'any|' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Union(List(Tree.Any)))
        tree should be(Tree.Any)
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(Empty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(Empty))
      }
      "should handle the 'void|' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Union(List(Tree.Void)))
        tree should be(Tree.Void)
        conts.p should be(Some(Empty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(Empty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the 'int|' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Union(List(Tree.Int)))
        tree should be(Tree.Int)
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the 'int|!int' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int))))
        tree should be(Tree.Any)
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(Empty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(Empty))
      }

    }

    "of unions" - {

      "should handle the 'int|!int' type" in {
        val alg = IntersectionAlgorithm.forGraph(Graph.fromTree(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int)))))
        alg.calculate()
      }

    }

  }

}
