package nz.rd.whiley

import org.scalatest._
import org.scalatest.prop.PropertyChecks

import Generators._

class ShrubberyAlgorithmsSpec extends FreeSpec with Matchers with Inside with PropertyChecks {

  "ShrubberyAlgorithms.garbageCollect" - {
    "should keep 'int' type unchanged" in {
      val sy = Shrubbery.empty
      sy.root = 0
      sy.shrubs += (0 -> Shrub.Int)
      ShrubberyAlgorithms.garbageCollect(sy)
      sy.root should be (0)
      sy.shrubs should be (Map(0 -> Shrub.Int))
    }
    "should remove extra type from 'int'+'int' shrubbery" in {
      val sy = Shrubbery.empty
      sy.root = 0
      sy.shrubs += (0 -> Shrub.Int, 1 -> Shrub.Int)
      ShrubberyAlgorithms.garbageCollect(sy)
      sy.root should be (0)
      sy.shrubs should be (Map(0 -> Shrub.Int))
    }
    "should keep '<int>' type unchanged" in {
      val sy = Shrubbery.empty
      sy.root = 0
      sy.shrubs += (0 -> Shrub.Product(List(1)), 1 -> Shrub.Int)
      ShrubberyAlgorithms.garbageCollect(sy)
      sy.root should be (0)
      sy.shrubs should be (Map(0 -> Shrub.Product(List(1)), 1 -> Shrub.Int))
    }
    "should keep 'µX.<X|int>' type unchanged" in {
      val sy = Shrubbery.empty
      sy.root = 0
      sy.shrubs += (0 -> Shrub.Product(List(0, 1)), 1 -> Shrub.Int)
      ShrubberyAlgorithms.garbageCollect(sy)
      sy.root should be (0)
      sy.shrubs should be (Map(0 -> Shrub.Product(List(0, 1)), 1 -> Shrub.Int))
    }
    "should remove extra type from '<int>'+'int' shrubbery" in {
      val sy = Shrubbery.empty
      sy.root = 1
      sy.shrubs += (0 -> Shrub.Product(List(1)), 1 -> Shrub.Int)
      ShrubberyAlgorithms.garbageCollect(sy)
      sy.root should be (1)
      sy.shrubs should be (Map(1 -> Shrub.Int))
    }
  }
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
  "ShrubberyAlgorithms.simplifyIdentity(Shrubbery)" - {
    "should accept same values" in {
      forAll(treeGen, valueGen) { (tree: Tree, value: Value) =>
        val orig: Shrubbery = Shrubbery.fromTree(tree)
        val origCheck: Boolean = orig.accepts(value)
        val simplified: Shrubbery = orig.copy
        ShrubberyAlgorithms.simplifyIdentities(simplified)
        val simplifiedCheck: Boolean = simplified.accepts(value)
        withClue(s"Original: $tree, simplified: $simplified") {
          simplifiedCheck should be(origCheck)
        }
      }
    }
  }
  "ShrubberyAlgorithms.removeNonterminatingLoops" - {
    "should keep 'int' type unchanged" in {
      val sy = Shrubbery.empty
      sy.root = 0
      sy.shrubs += (0 -> Shrub.Int)
      ShrubberyAlgorithms.removeNonterminatingLoops(sy)
      sy.root should be (0)
      sy.shrubs should be (Map(0 -> Shrub.Int))
    }
    "should change 'µX.<X>' to void" in {
      val sy = Shrubbery.empty
      sy.root = 0
      sy.shrubs += (0 -> Shrub.Product(List(0)))
      ShrubberyAlgorithms.removeNonterminatingLoops(sy)
      sy.root should be (0)
      sy.shrubs should be (Map(0 -> Shrub.Void))
    }
    "should accept same values" in {
      forAll(treeGen, valueGen) { (tree: Tree, value: Value) =>
        val orig: Shrubbery = Shrubbery.fromTree(tree)
        val origCheck: Boolean = orig.accepts(value)
        val removed: Shrubbery = orig.copy
        ShrubberyAlgorithms.removeNonterminatingLoops(removed)
        val removedCheck: Boolean = removed.accepts(value)
        withClue(s"Original: $tree, removed: $removed") {
          removedCheck should be(origCheck)
        }
      }
    }
  }
  "ShrubberyAlgorithms.convertToDNF" - {
    "should keep 'int' type unchanged" in {
      val sy = Shrubbery.empty
      sy.root = 0
      sy.shrubs += (0 -> Shrub.Int)
      ShrubberyAlgorithms.convertToDNF(sy)
      sy.root should be (0)
      sy.shrubs should be (Map(0 -> Shrub.Int))
    }
    "should change 'null&!null' to void" in {
      val sy = Shrubbery.empty
      sy.root = 0
      sy.shrubs += (0 -> Shrub.Intersection(List(Shrub.Null, Shrub.Negation(Shrub.Null))))
      ShrubberyAlgorithms.convertToDNF(sy)
      sy.root should be (0)
      sy.shrubs should be (Map(0 -> Shrub.Void))
    }
    "should change '!(|)' to any" in {
      val sy = Shrubbery.empty
      sy.root = 0
      sy.shrubs += (0 -> Shrub.Negation(Shrub.Union(Nil)))
      ShrubberyAlgorithms.convertToDNF(sy)
      sy.root should be (0)
      sy.shrubs should be (Map(0 -> Shrub.Any))
    }
    "(!(int|<>)&null&any&any) should accept null" in {
      val sy = Shrubbery.fromTree {
        import Tree._
        Intersection(List(Negation(Union(List(Int, Product(List())))), Null, Any, Any))
      }
      ShrubberyAlgorithms.convertToDNF(sy)
      sy.accepts(Value.Null) should be(true)
    }
    "!(int|<>) should accept null" in {
      val sy = Shrubbery.fromTree {
        import Tree._
        Negation(Union(List(Int, Product(List()))))
      }
      ShrubberyAlgorithms.convertToDNF(sy)
      sy.accepts(Value.Null) should be(true)
    }
    "should accept same values" in {
      forAll(treeGen, valueGen) { (tree: Tree, value: Value) =>
        val orig: Shrubbery = Shrubbery.fromTree(tree)
        val origCheck: Boolean = orig.accepts(value)
        val dnf: Shrubbery = orig.copy
        ShrubberyAlgorithms.convertToDNF(dnf)
        val dnfCheck: Boolean = dnf.accepts(value)
        withClue(s"Original: $tree, dnf: $dnf") {
          dnfCheck should be(origCheck)
        }
      }
    }
  }
  "ShrubberyAlgorithms.reduce" - {
    def checkReduction[A](tree: Tree)(check: Shrubbery => A): A = {
      val orig: Shrubbery = Shrubbery.fromTree(tree)
      val reduced: Shrubbery = orig.copy
      ShrubberyAlgorithms.reduce(reduced)
      forAll(valueGen) { value: Value =>
        withClue(s"Original: $tree, reduced: $reduced") {
          reduced.accepts(value) should be(orig.accepts(value))
        }
      }
      check(reduced)
    }
    "should reduce (|) to void" in {
      checkReduction(Tree.Union(Nil)) { sy: Shrubbery =>
        sy(sy.root) should be(Shrub.Void)
      }
    }
    "should reduce !(|) to any" in {
      checkReduction(Tree.Negation(Tree.Union(Nil))) { sy: Shrubbery =>
        sy(sy.root) should be(Shrub.Any)
      }
    }
    "should reduce int&!int to void" in {
      checkReduction(Tree.Intersection(List(Tree.Int, Tree.Negation(Tree.Int)))) { sy: Shrubbery =>
        sy(sy.root) should be(Shrub.Void)
      }
    }
//    "should reduce int|!int to any" in {
//      checkReduction(Tree.Union(List(Tree.Int, Tree.Negation(Tree.Int)))) { sy: Shrubbery =>
//        sy(sy.root) should be(Shrub.Any)
//      }
//    }
    "should reduce µX.(X|int) to int" in {
      checkReduction(Tree.Recursive("X", Tree.Union(List(Tree.Variable("X"), Tree.Int)))) { sy: Shrubbery =>
        sy(sy.root) should be(Shrub.Int)
      }
    }
    "should reduce µX.!X to any" in {
      checkReduction(Tree.Recursive("X", Tree.Negation(Tree.Variable("X")))) { sy: Shrubbery =>
        sy(sy.root) should be(Shrub.Any)
      }
    }
    "should reduce !(µX.!X) to void" in {
      checkReduction(Tree.Negation(Tree.Recursive("X", Tree.Negation(Tree.Variable("X"))))) { sy: Shrubbery =>
        sy(sy.root) should be(Shrub.Void)
      }
    }
    "should reduce !(|(µX.!X)) to void" in {
      checkReduction(Tree.Negation(Tree.Union(List(Tree.Recursive("X", Tree.Negation(Tree.Variable("X"))))))) { sy: Shrubbery =>
        sy(sy.root) should be(Shrub.Void)
      }
    }
    "should reduce µX.<X> to void" in {
      checkReduction(Tree.Recursive("X", Tree.Product(List(Tree.Variable("X"))))) { sy: Shrubbery =>
        sy(sy.root) should be(Shrub.Void)
      }
    }
    "should reduce µX.<int,X> to void" in {
      checkReduction(Tree.Recursive("X", Tree.Product(List(Tree.Int, Tree.Variable("X"))))) { sy: Shrubbery =>
        sy(sy.root) should be(Shrub.Void)
      }
    }
    "should accept same values" in {
      forAll(treeGen, valueGen) { (tree: Tree, value: Value) =>
        val orig: Shrubbery = Shrubbery.fromTree(tree)
        val origCheck: Boolean = orig.accepts(value)
        val reduced: Shrubbery = orig.copy
        ShrubberyAlgorithms.reduce(reduced)
        val reducedCheck: Boolean = reduced.accepts(value)
        withClue(s"Original: $tree, reduced: $reduced") {
          reducedCheck should be(origCheck)
        }
      }
    }
  }
}
