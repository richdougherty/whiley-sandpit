package nz.rd.whiley

import org.scalatest._
import scala.collection.mutable

class IntersectionSpec extends FreeSpec with Matchers {

  "Intersections" - {
    "should handle the 'int|!int' type" in {
      val alg = new IntersectionAlgorithm(Graph.fromTree(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int)))), mutable.Map.empty)
      alg.calculate()
    }
  }

}
