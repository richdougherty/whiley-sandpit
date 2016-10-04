package nz.rd.whiley

import org.scalatest._
import org.scalatest.prop.PropertyChecks

import Generators._
import Shrub.Ref

class ShrubberyAlgorithmsSpec extends FreeSpec with Matchers with Inside with PropertyChecks {

  "ShrubberyAlgorithms.simplifyIdentity(Shrub)" - {
    "should simplify any shrub to any" in {
      ShrubberyAlgorithms.simplifyIdentities(Shrub.Any) should be(Shrub.Any)
    }
    "should simplify void shrub to void" in {
      ShrubberyAlgorithms.simplifyIdentities(Shrub.Void) should be(Shrub.Void)
    }
    "should simplify null shrub" in {
      ShrubberyAlgorithms.simplifyIdentities(Shrub.Null) should be(Shrub.Null)
    }
    "should simplify int shrub to int" in {
      ShrubberyAlgorithms.simplifyIdentities(Shrub.Int) should be(Shrub.Int)
    }
    "should simplify int|any shrub to any" in {
      ShrubberyAlgorithms.simplifyIdentities(Shrub.Union(List(Shrub.Int, Shrub.Any))) should be(Shrub.Any)
    }
    "should simplify int|void shrub to int" in {
      ShrubberyAlgorithms.simplifyIdentities(Shrub.Union(List(Shrub.Int, Shrub.Void))) should be(Shrub.Int)
    }
    "should simplify !!int shrub" in {
      ShrubberyAlgorithms.simplifyIdentities(Shrub.Negation(Shrub.Negation(Shrub.Int))) should be(Shrub.Int)
    }
  }
  "ShrubberyAlgorithms.simplifyIdentity(Ref)" - {
    "should accept same values" in {
      forAll(treeGen, valueGen) { (tree: Tree, value: Value) =>
        val orig: Ref = Ref(Shrub.fromTree(tree))
        val origCheck: Boolean = orig.get.accepts(value)
        val simplified: Ref = Ref(Shrub.fromTree(tree))
        ShrubberyAlgorithms.simplifyIdentities(simplified)
        val simplifiedCheck: Boolean = simplified.get.accepts(value)
        withClue(s"Original: $tree, simplified: $simplified") {
          simplifiedCheck should be(origCheck)
        }
      }
    }
  }
  "ShrubberyAlgorithms.removeNonterminatingLoops" - {
    "should keep 'int' type unchanged" in {
      val ref = Ref(Shrub.Int)
      ShrubberyAlgorithms.removeNonterminatingLoops(ref)
      ref.get should be (Shrub.Int)
    }
    "should change 'µX.<X>' to void" in {
      val ref: Ref = Ref.empty()
      ref.set(Shrub.Product(List(ref)))
      ShrubberyAlgorithms.removeNonterminatingLoops(ref)
      ref.get should be (Shrub.Void)
    }
    "should accept same values" in {
      forAll(treeGen, valueGen) { (tree: Tree, value: Value) =>
        val orig: Ref = Ref(Shrub.fromTree(tree))
        val origCheck: Boolean = orig.get.accepts(value)
        val removed: Ref = Ref(Shrub.fromTree(tree))
        ShrubberyAlgorithms.removeNonterminatingLoops(removed)
        val removedCheck: Boolean = removed.get.accepts(value)
        withClue(s"Original: $tree, removed: $removed") {
          removedCheck should be(origCheck)
        }
      }
    }
  }
  "ShrubberyAlgorithms.convertToDNF" - {
    "should keep 'int' type unchanged" in {
      val ref = Ref(Shrub.Int)
      ShrubberyAlgorithms.convertToDNF(ref)
      ref.get should be (Shrub.Int)
    }
    "should change 'null&!null' to void" in {
      val ref = Ref(Shrub.Intersection(List(Shrub.Null, Shrub.Negation(Shrub.Null))))
      ShrubberyAlgorithms.convertToDNF(ref)
      ref.get should be (Shrub.Void)
    }
    "should change '!(|)' to any" in {
      val ref = Ref(Shrub.Negation(Shrub.Union(Nil)))
      ShrubberyAlgorithms.convertToDNF(ref)
      ref.get should be (Shrub.Any)
    }
    "(!(int|<>)&null&any&any) should accept null" in {
      val ref = Ref(Shrub.Intersection(List(Shrub.Negation(Shrub.Union(List(Shrub.Int, Shrub.Product(List())))), Shrub.Null, Shrub.Any, Shrub.Any)))
      ShrubberyAlgorithms.convertToDNF(ref)
      ref.get.accepts(Value.Null) should be(true)
    }
    "!(int|<>) should accept null" in {
      val ref = Ref(Shrub.Negation(Shrub.Union(List(Shrub.Int, Shrub.Product(List())))))
      ShrubberyAlgorithms.convertToDNF(ref)
      ref.get.accepts(Value.Null) should be(true)
    }
    "should accept same values" in {
      forAll(treeGen, valueGen) { (tree: Tree, value: Value) =>
        val orig: Ref = Ref(Shrub.fromTree(tree))
        val origCheck: Boolean = orig.get.accepts(value)
        val dnf: Ref = Ref(Shrub.fromTree(tree))
        ShrubberyAlgorithms.convertToDNF(dnf)
        val dnfCheck: Boolean = dnf.get.accepts(value)
        withClue(s"Original: $tree, dnf: $dnf") {
          dnfCheck should be(origCheck)
        }
      }
    }
  }
  "ShrubberyAlgorithms.reduce" - {
    def checkReduction[A](tree: Tree)(check: Ref => A): A = {
      val orig: Ref = Ref(Shrub.fromTree(tree))
      val reduced: Ref = Ref(Shrub.fromTree(tree))
      ShrubberyAlgorithms.reduce(reduced)
      forAll(valueGen) { value: Value =>
        withClue(s"Original: $tree, reduced: $reduced") {
          reduced.get.accepts(value) should be(orig.get.accepts(value))
        }
      }
      check(reduced)
    }
    "should reduce (|) to void" in {
      checkReduction(Tree.Union(Nil)) { ref: Ref =>
        ref.get should be(Shrub.Void)
      }
    }
    "should reduce !(|) to any" in {
      checkReduction(Tree.Negation(Tree.Union(Nil))) { ref: Ref =>
        ref.get should be(Shrub.Any)
      }
    }
    "should reduce int&!int to void" in {
      checkReduction(Tree.Intersection(List(Tree.Int, Tree.Negation(Tree.Int)))) { ref: Ref =>
        ref.get should be(Shrub.Void)
      }
    }
    "should reduce int|!int to any" in {
      checkReduction(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int)))) { ref: Ref =>
        ref.get should be(Shrub.Any)
      }
    }
    "should reduce µX.(X|int) to int" in {
      checkReduction(Tree.Recursive("X", Tree.Union(List(Tree.Variable("X"), Tree.Int)))) { ref: Ref =>
        ref.get should be(Shrub.Int)
      }
    }
    "should reduce µX.!X to any" in {
      checkReduction(Tree.Recursive("X", Tree.Negation(Tree.Variable("X")))) { ref: Ref =>
        ref.get should be(Shrub.Any)
      }
    }
    "should reduce !(µX.!X) to void" in {
      checkReduction(Tree.Negation(Tree.Recursive("X", Tree.Negation(Tree.Variable("X"))))) { ref: Ref =>
        ref.get should be(Shrub.Void)
      }
    }
    "should reduce !(|(µX.!X)) to void" in {
      checkReduction(Tree.Negation(Tree.Union(List(Tree.Recursive("X", Tree.Negation(Tree.Variable("X"))))))) { ref: Ref =>
        ref.get should be(Shrub.Void)
      }
    }
    "should reduce µX.<X> to void" in {
      checkReduction(Tree.Recursive("X", Tree.Product(List(Tree.Variable("X"))))) { ref: Ref =>
        ref.get should be(Shrub.Void)
      }
    }
    "should reduce µX.<int,X> to void" in {
      checkReduction(Tree.Recursive("X", Tree.Product(List(Tree.Int, Tree.Variable("X"))))) { ref: Ref =>
        ref.get should be(Shrub.Void)
      }
    }
    "should accept same values" in {
      forAll(treeGen, valueGen) { (tree: Tree, value: Value) =>
        val orig: Ref = Ref(Shrub.fromTree(tree))
        val origCheck: Boolean = orig.get.accepts(value)
        val reduced: Ref = Ref(Shrub.fromTree(tree))
        ShrubberyAlgorithms.reduce(reduced)
        val reducedCheck: Boolean = reduced.get.accepts(value)
        withClue(s"Original: $tree, reduced: $reduced") {
          reducedCheck should be(origCheck)
        }
      }
    }
  }
}
