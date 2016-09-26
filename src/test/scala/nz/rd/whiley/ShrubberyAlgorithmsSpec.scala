package nz.rd.whiley

import org.scalatest._
import org.scalatest.prop.PropertyChecks

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

}
