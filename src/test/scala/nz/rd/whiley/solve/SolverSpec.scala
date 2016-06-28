package nz.rd.whiley.solve

import org.scalatest._

class SolverSpec extends FreeSpec with Matchers {

  "Solver.empty" - {
    "should return an empty list of solutions" in {
      Solver.empty[Unit].solve(()) should be(Solutions(Nil))
    }
  }

  "Solver.const" - {
    "should return a single solution" in {
      Solver.const[String,Int](1).solve("context") should be(Solutions(List(Solution("context", 1))))
    }
  }

  "Solver.map" - {
    "should map a single solution" in {
      Solver.const[String,Int](1).map[Int](_ * 2).solve("context") should be(Solutions(List(Solution("context", 2))))
    }
  }

  "Solver.flatMap" - {
    "should flatMap a single solution" in {
      Solver.const[String,Int](1).flatMap[Int](x => Solver.const[String,Int](x * 3)).solve("foo") should be(Solutions(List(Solution("foo", 3))))
    }
  }

  "Solver.++" - {
    "should combine solutions" in {
      (Solver.const[String,Int](1) ++ Solver.const[String,Int](2)).solve("x") should be(Solutions(List(Solution("x", 1), Solution("x", 2))))
    }
  }

  "Solver.getContext" - {
    "should extract the context" in {
      Solver.getContext[String].solve("x") should be(Solutions(List(Solution("x", "x"))))
    }
  }

}
