package nz.rd.whiley

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class IntersectionSpec extends FreeSpec with PropertyChecks with Matchers {

  import Graph.Node

  "Intersections" - {

    "simplify graph properly" - {
      "for void variant #1" in {
        val g = Graph(2, Map(
          0 -> Node.Union(List(8, 1)),
          1 -> Node.Null,
          2 -> Node.Void,
          3 -> Node.Negation(7),
          4 -> Node.Null,
          5 -> Node.Any,
          6 -> Node.Union(List(8, 8, 1, 4, 0)),
          7 -> Node.Negation(7),
          8 -> Node.Negation(9),
          9 -> Node.Null
        ))
        val alg = IntersectionAlgorithm.forGraph(g)
        alg.calculate()
        g.toTree should be(Tree.Void)
      }
      "for void variant #2" in {
        val g = Graph(15, Map(
          0 -> Node.Union(List(8, 6, 2, 1, 20, 6, 13, 18, 21, 18, 8)),
          1 -> Node.Negation(18),
          2 -> Node.Null,
          3 -> Node.Null,
          4 -> Node.Void,
          5 -> Node.Any,
          6 -> Node.Int,
          7 -> Node.Null,
          8 -> Node.Negation(2),
          9 -> Node.Null,
          10 -> Node.Negation(9),
          11 -> Node.Int,
          12 -> Node.Int,
          13 -> Node.Int,
          14 -> Node.Negation(7),
          15 -> Node.Void,
          16 -> Node.Int,
          17 -> Node.Negation(2),
          18 -> Node.Null,
          19 -> Node.Void,
          20 -> Node.Union(List(8, 22, 13, 16, 1, 19, 22, 20, 12, 3, 4, 3, 16, 2, 8, 15)),
          21 -> Node.Any,
          22 -> Node.Null,
          23 -> Node.Int
        ))
        val alg = IntersectionAlgorithm.forGraph(g)
        alg.calculate()
        g.toTree should be(Tree.Void)
      }
      "for !(|) variant #1" in {
        val g = Graph(0, Map(
          0 -> Node.Negation(1),
          1 -> Node.Union(List())
        ))
        val alg = IntersectionAlgorithm.forGraph(g)
        alg.calculate()
        g.toTree should be(Tree.Any)
      }
      "for !(µX.!X) variant #1" in {
        val g = Graph(0, Map(
          0 -> Node.Negation(1),
          1 -> Node.Negation(1)
        ))
        val alg = IntersectionAlgorithm.forGraph(g)
        alg.calculate()
        g.toTree should be(Tree.Void)
      }
      "for !(|(µX.!X)) variant #1" in {
        val g = Graph(0, Map(
          0 -> Node.Negation(1),
          1 -> Node.Union(List(2)),
          2 -> Node.Negation(2)
        ))
        val alg = IntersectionAlgorithm.forGraph(g)
        alg.calculate()
        g.toTree should be(Tree.Void)
      }
      "for !null variant #1" in {
        val g = Graph(0, Map(
          0 -> Node.Negation(1),
          1 -> Node.Null
        ))
        val alg = IntersectionAlgorithm.forGraph(g)
        alg.calculate()
        g.toTree should be(Tree.Negation(Tree.Null))
      }
      "for !null variant #2" in {
        val g = Graph(16, Map(
          0 -> Node.Negation(10),
          1 -> Node.Union(List(11, 7, 4, 9, 2, 11, 1)),
          2 -> Node.Int,
          3 -> Node.Int,
          4 -> Node.Void,
          5 -> Node.Negation(11),
          6 -> Node.Union(List(1, 9, 0, 16, 9, 5)),
          7 -> Node.Void,
          8 -> Node.Union(List(9, 15, 11, 13, 14, 7, 9, 6, 2, 14, 9, 16, 11, 10)),
          9 -> Node.Union(List(6, 13, 6, 9, 11, 13, 11, 5, 16, 2)),
          10 -> Node.Null,
          11 -> Node.Negation(8),
          12 -> Node.Any,
          13 -> Node.Void,
          14 -> Node.Int,
          15 -> Node.Void,
          16 -> Node.Negation(10)
        ))
        val alg = IntersectionAlgorithm.forGraph(g)
        alg.calculate()
        g.toTree should be(Tree.Negation(Tree.Null))
      }
    }

    "of the root with itself" - {

      def runIntersection(tree: Tree): (Tree, Contents, Intersections) = {
        val alg = IntersectionAlgorithm.forGraph(Graph.fromTree(tree))
        alg.calculate()
        val newTree = alg.g.toTree
        val rootConts = alg.getContents(alg.g.root)
        val rootInts = alg.getInts(alg.g.root, alg.g.root)
        forAll(Generators.valueGen) { value =>
          val origCheck = TypeChecker.check(value, tree)
          val newCheck = TypeChecker.check(value, newTree)
          withClue(s"Original: $tree, simplified: $newTree") {
            newCheck should be(origCheck)
          }
        }
        (newTree, rootConts, rootInts)
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
      "should handle the 'null' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Null)
        tree should be(Tree.Null)
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
      "should handle the '!null' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Negation(Tree.Null))
        tree should be(Tree.Negation(Tree.Null))
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
      "should handle the '!(|)' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Negation(Tree.Union(Nil)))
        tree should be(Tree.Any)
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(Empty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(Empty))
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
      "should handle the 'µX.(X|int)' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Recursive("X0", Tree.Union(List(Tree.Variable("X0"), Tree.Int))))
        tree should be(Tree.Int)
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the 'int|(int|)' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Union(List(Tree.Int, Tree.Union(List(Tree.Int)))))
        tree should be(Tree.Int)
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the '<int>' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Product(List(Tree.Int)))
        tree should be(Tree.Product(List(Tree.Int)))
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }
      "should handle the '!µX.(int|X)' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Negation(Tree.Recursive("X", Tree.Union(List(Tree.Int, Tree.Variable("X"))))))
        tree should be(Tree.Negation(Tree.Recursive("X0", Tree.Union(List(Tree.Int, Tree.Variable("X"))))))
        conts.p should be(Some(NonEmpty))
        conts.n should be(Some(NonEmpty))
        ints.pp should be(Some(NonEmpty))
        ints.pn should be(Some(Empty))
        ints.np should be(Some(Empty))
        ints.nn should be(Some(NonEmpty))
      }

    }

    "of unions" - {

      "should handle the 'int|!int' type" in {
        val alg = IntersectionAlgorithm.forGraph(Graph.fromTree(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int)))))
        alg.calculate()
      }

    }

    "original and simplified forms of generated types must match same values" - {

      import Generators._

      "for generated trees" in {
        forAll(treeGen, valueGen) { (tree: Tree, value: Value) =>
          val origCheck: Boolean = TypeChecker.check(value, tree)
          val g = Graph.fromTree(tree)
          val alg = IntersectionAlgorithm.forGraph(g)
          alg.calculate()
          val newTree = g.toTree
          val newCheck: Boolean = TypeChecker.check(value, g)
          withClue(s"Original: $tree, simplified: $newTree") {
            newCheck should be(origCheck)
          }
        }
      }

      "for generated graphs" in {
        forAll(graphGen, valueGen) { (graph: Graph, value: Value) =>
          val origGraphDebug: String = graph.toString
          val origCheck: Boolean = TypeChecker.check(value, graph)
          val alg = IntersectionAlgorithm.forGraph(graph)
          alg.calculate()
          val simplifiedGraphDebug: String = graph.toString
          val newCheck: Boolean = TypeChecker.check(value, graph)
          withClue(s"Original: $origGraphDebug, simplified: $simplifiedGraphDebug") {
            newCheck should be(origCheck)
          }
        }
      }

    }

  }

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = 1000)

}
