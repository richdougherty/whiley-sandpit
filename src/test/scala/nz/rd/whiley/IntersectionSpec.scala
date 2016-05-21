package nz.rd.whiley

import org.scalacheck._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scala.collection.mutable

class IntersectionSpec extends FreeSpec with PropertyChecks with Matchers {

  import Graph.{Id, Node}

  val valueGen: Gen[Value] = Gen.frequency(
    5 -> Gen.const(Value.Null),
    5 -> Arbitrary.arbitrary[Int].map(Value.Int(_)),
    2 -> Gen.const(Value.Record(Nil)),
    1 -> Gen.lzy {
      for {
        x <- valueGen
      } yield Value.Record(List("x" -> x))
    },
    1 -> Gen.lzy {
      for {
        x <- valueGen
        y <- valueGen
      } yield Value.Record(List("x" -> x, "y" -> y))
    }
  )

  val graphGen: Gen[Graph] = Gen.sized { sizeMinusOne =>
    import Graph.Node
    val size = sizeMinusOne + 1 // Ensure minimum size is 1
    val idGen: Gen[Graph.Id] = Gen.choose(0, size-1)
    val nodeGen: Gen[Node] = Gen.frequency(
      1 -> Node.Any,
      1 -> Node.Void,
      1 -> Node.Null,
      1 -> Node.Int,
      1 -> idGen.map(Node.Negation(_)),
      1 -> Gen.listOf(idGen).map(Node.Union(_))
    )
    for {
      root <- idGen
      nodes <- Gen.listOfN(size, nodeGen)
    } yield {
      val g = Graph.empty
      g.root = root
      for ((node, id) <- nodes.zipWithIndex) { g += (id -> node)}
      g
    }
  }

  "Intersections" - {

    "simplify graph properly" - {
      "example 1" in {
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
      "example 2" in {
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
      "example 3" in {
        val g = Graph(0, Map(
          0 -> Node.Negation(1),
          1 -> Node.Union(List())
        ))
        val alg = IntersectionAlgorithm.forGraph(g)
        alg.calculate()
        g.toTree should be(Tree.Any)
      }
    }

    "of the root with itself" - {

      def runIntersection(tree: Tree): (Tree, Contents, Intersections) = {
        val alg = IntersectionAlgorithm.forGraph(Graph.fromTree(tree))
        alg.calculate()
        val newTree = alg.g.toTree
        val rootConts = alg.getContents(alg.g.root)
        val rootInts = alg.getInts(alg.g.root, alg.g.root)
        forAll(valueGen) { value =>
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
      "should handle the '{x: int}' type" in {
        val (tree, conts, ints) = runIntersection(Tree.Record(List(("x", Tree.Int))))
        tree should be(Tree.Record(List(("x", Tree.Int))))
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

    "original and simplified forms of generated types must match same values" in {
      forAll(graphGen) { graph =>
        val origGraph = graph.copy
        val alg = IntersectionAlgorithm.forGraph(graph)
        alg.calculate()
        // withClue(s"Original: $origGraph, simplified: $graph") {
        forAll(valueGen) { value =>
          val origCheck = TypeChecker.check(value, origGraph)
          val newCheck = TypeChecker.check(value, graph)
          withClue(s"Original: $origGraph, simplified: $graph") {
            newCheck should be(origCheck)
          }
        }
        // }
      }
    }

  }

}
