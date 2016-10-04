package nz.rd.whiley

import nz.rd.whiley.Shrub.Ref
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class ShrubSpec extends FreeSpec with Matchers with Inside with PropertyChecks {

  "Shrubbery.fromTree" - {
    "should handle the 'any' type" in {
      val ref: Ref = Ref(Shrub.fromTree(Tree.Any))
      ref.get should be (Shrub.Any)
    }
    "should handle the 'void' type" in {
      val ref: Ref = Ref(Shrub.fromTree(Tree.Void))
      ref.get should be (Shrub.Void)
    }
    "should handle the 'null' type" in {
      val ref: Ref = Ref(Shrub.fromTree(Tree.Null))
      ref.get should be (Shrub.Null)
    }
    "should handle the 'int' type" in {
      val ref: Ref = Ref(Shrub.fromTree(Tree.Int))
      ref.get should be (Shrub.Int)
    }
    "should handle the '!int' type" in {
      val ref: Ref = Ref(Shrub.fromTree(Tree.Negation(Tree.Int)))
      ref.get should be (Shrub.Negation(Shrub.Int))
    }
    "should handle the 'int|!int' type" in {
      val ref: Ref = Ref(Shrub.fromTree(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int)))))
      ref.get should be (Shrub.Union(List(Shrub.Int, Shrub.Negation(Shrub.Int))))
    }
    "should handle the 'µX.X' type" in {
      val ref: Ref = Ref(Shrub.fromTree(Tree.Recursive("X0", Tree.Variable("X0"))))
      ref.get should be (Shrub.Void)
    }
    "should handle the 'µX.X|int' type" in {
      val ref: Ref = Ref(Shrub.fromTree(Tree.Recursive("X0", Tree.Union(List(Tree.Variable("X0"), Tree.Int)))))
      ref.get should be (Shrub.Union(List(Shrub.Void, Shrub.Int)))
    }
    "should handle the '<int, int>' type" in {
      val ref: Ref = Ref(Shrub.fromTree(Tree.Product(List(Tree.Int, Tree.Int))))
      inside(ref.get) {
        case Shrub.Product(List(ref2, ref3)) =>
          ref2.get should be (Shrub.Int)
          ref3.get should be (Shrub.Int)
      }
    }
    "should handle the 'µX.<X,int>' type" in {
      val ref: Ref = Ref(Shrub.fromTree(Tree.Recursive("X0", Tree.Product(List(Tree.Variable("X0"), Tree.Int)))))
      inside(ref.get) {
        case Shrub.Product(List(ref2, ref3)) =>
          ref2 should be (ref)
          ref3.get should be (Shrub.Int)
      }
    }

    def checkFromTree(tree: Tree, value: Value) = {
      val treeCheck: Boolean = TypeChecker.check(value, tree)
      val shrubRef: Ref = Ref(Shrub.fromTree(tree))
      val shrubCheck: Boolean = shrubRef.get.accepts(value)
      withClue(s"Original tree: $tree, converted shrub: $shrubRef") {
        shrubCheck should be(treeCheck)
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
    "should not accept <> as a value of type void" in {
      Shrub.Void.accepts(Value.Product(Nil)) should be (false)
    }
    "should not accept <> as a value of type <<>>" in {
      Shrub.fromTree(Tree.Product(List(Tree.Product(List())))).get.accepts(Value.Product(Nil)) should be (false)
    }
    "should not accept <1> as a value of type <<>>" in {
      Shrub.fromTree(Tree.Product(List(Tree.Product(List())))).get.accepts(Value.Product(List(Value.Int(1)))) should be (false)
    }
    "should accept <1> as a value of type <int>" in {
      Shrub.fromTree(Tree.Product(List(Tree.Int))).get.accepts(Value.Product(List(Value.Int(1)))) should be (true)
    }
    "should accept null and int as values of type µX.!X" in {
      val tree = Tree.Recursive("X",Tree.Negation(Tree.Variable("X")))
//      TypeChecker.check(Value.Null, tree) should be(true)
//      TypeChecker.check(Value.Int(1), tree) should be(true)
      val ref = Shrub.fromTree(tree)
      ref.get.accepts(Value.Null) should be(true)
      ref.get.accepts(Value.Int(1)) should be(true)
    }

  }

}
