package nz.rd.whiley.solve

final case class Solutions[+C,+A](list: List[Solution[C,A]]) {
  def results: List[A] = list.map(_.value)
  def map[D,B](f: C=>D, g: A=>B): Solutions[D,B] = {
    Solutions(list.map {
      case Solution(context, value) => Solution(f(context), g(value))
    })
  }
  def ++[C1>:C,A1>:A](that: Solutions[C1,A1]): Solutions[C1,A1] = Solutions(this.list ++ that.list)
}

object Solutions {
  def empty: Solutions[Nothing,Nothing] = Solutions(Nil)
  def single[C,A](context: C, value: A) = Solutions(List(Solution(context, value)))
}
