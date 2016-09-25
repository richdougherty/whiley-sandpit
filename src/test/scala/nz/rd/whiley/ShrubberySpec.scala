package nz.rd.whiley

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class ShrubberySpec extends FreeSpec with Matchers with Inside with PropertyChecks {

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

    def checkFromTree(tree: Tree, value: Value) = {
      val treeCheck: Boolean = TypeChecker.check(value, tree)
      val shrubbery: Shrubbery = Shrubbery.fromTree(tree)
      val shrubberyCheck: Boolean = shrubbery.accepts(value)
      withClue(s"Original: $tree, simplified: $shrubbery") {
        shrubberyCheck should be(treeCheck)
      }
    }

    "should accept same values as original Tree '<<>>'" in {
      checkFromTree(Tree.Product(List(Tree.Product(List()))), Value.Product(List(Value.Int(1))))
    }

    "should accept same values as original Tree" in {
      forAll(Generators.treeGen, Generators.valueGen)(checkFromTree(_, _))
    }
  }
  "Shrubbery.accepts" - {
    "should not accept <> as a value of type <<>>" in {
      Shrubbery.fromTree(Tree.Product(List(Tree.Product(List())))).accepts(Value.Product(Nil)) should be (false)
    }
    "should not accept <1> as a value of type <<>>" in {
      Shrubbery.fromTree(Tree.Product(List(Tree.Product(List())))).accepts(Value.Product(List(Value.Int(1)))) should be (false)
    }
    "should accept <1> as a value of type <int>" in {
      Shrubbery.fromTree(Tree.Product(List(Tree.Int))).accepts(Value.Product(List(Value.Int(1)))) should be (true)
    }

  }

}
