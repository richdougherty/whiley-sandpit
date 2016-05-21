package nz.rd.whiley

import org.scalatest._
import scala.collection.mutable

class IntersectionSpec extends FreeSpec with Matchers {

  val exampleValues = Vector[Value](
    Value.Null,
    Value.Int(-1),
    Value.Int(0),
    Value.Int(1),
    Value.Int(100),
    Value.Null,
    Value.Record(List()),
    Value.Record(List(("x", Value.Int(1)))),
    Value.Record(List(("x", Value.Null)))
  )

  "Intersections" - {

    "of the root with itself" - {

      def runIntersection(tree: Tree): (Tree, Contents, Intersections) = {
        // println(s"--- Running intersection on $t ---")
        val alg = IntersectionAlgorithm.forGraph(Graph.fromTree(tree))
        alg.calculate()
        // println(s"Alg: nodes: ${alg.g.root}, ${alg.g.nodes}, ${alg.contents}, ${alg.intersections}")
        val newTree = alg.g.toTree
        val rootConts = alg.getContents(alg.g.root)
        val rootInts = alg.getInts(alg.g.root, alg.g.root)
        for (value <- exampleValues) {
          val origCheck = TypeChecker.check(value, tree)
          val newCheck = TypeChecker.check(value, newTree)
          withClue(s"Checking $value: ") {
            origCheck should be(newCheck)
          }
        }
        // println(s"--- Done ---")
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

  }

}
