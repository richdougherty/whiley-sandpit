package nz.rd.whiley.solve

trait Solver[C,+A] {
  parent =>
  def solve(context: C): Solutions[C,A]
  def map[B](f: A => B): Solver[C,B] = new Solver[C,B] {
    override def solve(context: C): Solutions[C,B] = {
      val ss: Solutions[C,A] = parent.solve(context)
      ss.map(identity[C], f)
    }
  }
  def flatMap[B](f: A => Solver[C,B]): Solver[C,B] = new Solver[C,B] {
    override def solve(context: C): Solutions[C,B] = {
      val ss1: List[Solution[C,A]] = parent.solve(context).list
      val ss2: List[Solution[C,B]] = ss1.flatMap {
        case Solution(context, value) =>
          val nested: Solver[C,B] = f(value)
          nested.solve(context).list
      }
      Solutions(ss2)
    }
  }
  def ++[A1>:A](that: Solver[C,A1]): Solver[C,A1] = new Solver[C,A1] {
    override def solve(context: C): Solutions[C,A1] = {
      val ss1 = parent.solve(context)
      val ss2 = that.solve(context)
      ss1 ++ ss2
    }
  }
  def changeContext[D](enter: D => C, exit: C => D): Solver[D,A] = new Solver[D,A] {
    override def solve(context: D): Solutions[D,A] = {
      parent.solve(enter(context)).map(exit, identity[A])
    }
  }
//  def foreach(f: A => Unit): Solver[C,Unit] = new Solver[C,Unit] {
//    def solve(context: C): Solutions[C,Nothing] = {
//      parent.solve(context).list.foreach {
//        case Solution(_, value) => f(value)
//      }
//      Solutions[C,Nothing](Nil)
//    }
//  }
}

object Solver {
  def apply[C,A](f: C => Solutions[C,A]) = new Solver[C,A] {
    override def solve(context: C): Solutions[C,A] = f(context)
  }
  def empty[C] = new Solver[C,Nothing] {
    override def solve(context: C): Solutions[C,Nothing] = Solutions.empty
  }
  def const[C,A](value: A) = new Solver[C,A] {
    override def solve(context: C): Solutions[C,A] = Solutions.single(context, value)
  }
  def getContext[C] = new Solver[C,C] {
    override def solve(context: C): Solutions[C,C] = Solutions.single(context, context)
  }

  def foldLeft[C,A,B](list: List[A], z: B)(op: (B, A) => Solver[C,B]): Solver[C,B] = {
    list match {
      case Nil => Solver.const(z)
      case head::tail =>
        for {
          accumulator <- op(z, head)
          tailResult <- foldLeft(tail, accumulator)(op)
        } yield tailResult
    }
  }
//    override def solve(context: C): Solutions[C,B] = {
//
//    }
//    var acc = z
//    var these = this
//    while (!these.isEmpty) {
//      acc = op(acc, these.head)
//      these = these.tail
//    }
//    acc
//  }
//  def transformContext[C,D,A](
//      inner: Solver[D,A],
//      wrap: C => D,
//      unwrap: D => C): Solver[C,A] = {
//    new Solver[C, Ternary] {
//      override def solve(context: C): Solutions[D, A] = {
//        inner.solve(wrap(context)).map(unwrap, identity[A])
//      }
//    }
//  }

}