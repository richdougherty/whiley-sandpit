package nz.rd.whiley

import org.scalatest._

class ShrubberySpec extends FreeSpec with Matchers with Inside {

  "Shrubbery.fromTree" - {
    "should handle the 'any' type" in {
      val sy: Shrubbery  = Shrubbery.fromTree(Tree.Any)
      sy(sy.root) should be (Shrub.Any)
    }
    "should handle the 'void' type" in {
      val sy: Shrubbery  = Shrubbery.fromTree(Tree.Void)
      sy(sy.root) should be (Shrub.Void)
    }
    "should handle the 'null' type" in {
      val sy: Shrubbery  = Shrubbery.fromTree(Tree.Null)
      sy(sy.root) should be (Shrub.Null)
    }
    "should handle the 'int' type" in {
      val sy: Shrubbery  = Shrubbery.fromTree(Tree.Int)
      sy(sy.root) should be (Shrub.Int)
    }
    "should handle the '!int' type" in {
      val sy: Shrubbery  = Shrubbery.fromTree(Tree.Negation(Tree.Int))
      sy(sy.root) should be (Shrub.Negation(Shrub.Int))
    }
    "should handle the 'int|!int' type" in {
      val sy: Shrubbery  = Shrubbery.fromTree(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int))))
      sy(sy.root) should be (Shrub.Union(List(Shrub.Int, Shrub.Negation(Shrub.Int))))
    }
    "should handle the 'µX.X' type" in {
      val sy: Shrubbery  = Shrubbery.fromTree(Tree.Recursive("X0", Tree.Variable("X0")))
      sy(sy.root) should be (Shrub.Void)
    }
    "should handle the 'µX.X|int' type" in {
      val sy: Shrubbery  = Shrubbery.fromTree(Tree.Recursive("X0", Tree.Union(List(Tree.Variable("X0"), Tree.Int))))
      sy(sy.root) should be (Shrub.Union(List(Shrub.Void, Shrub.Int)))
    }
    "should handle the '<int, int>' type" in {
      val sy: Shrubbery  = Shrubbery.fromTree(Tree.Product(List(Tree.Int, Tree.Int)))
      inside(sy(sy.root)) {
        case Shrub.Product(List(sid2, sid3)) =>
          sy(sid2) should be (Shrub.Int)
          sy(sid3) should be (Shrub.Int)
      }
    }
    "should handle the 'µX.<X,int>' type" in {
      val sy: Shrubbery  = Shrubbery.fromTree(Tree.Recursive("X0", Tree.Product(List(Tree.Variable("X0"), Tree.Int))))
      inside(sy(sy.root)) {
        case Shrub.Product(List(sid2, sid3)) =>
          sid2 should be (sy.root)
          sy(sid3) should be (Shrub.Int)
      }
    }
  }

}
