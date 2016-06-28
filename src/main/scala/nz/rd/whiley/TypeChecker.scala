package nz.rd.whiley

import nz.rd.whiley.Graph.{Id, Node}
import nz.rd.whiley.TypeChecker2.Heap
import nz.rd.whiley.solve.{Solutions, Solver}

object TypeChecker {
  
  def check(v: Value, t: Tree): Boolean = check(v, Graph.fromTree(t))
  def check(v: Value, g: Graph): Boolean = {
    val heap = new TypeChecker2.Heap[Value] {
      override def assert(fact: TypeChecker2.Fact[Value]): Solver[Heap[Value], Unit] = new Solver[Heap[Value], Unit] {
        override def solve(heap: Heap[Value]): Solutions[Heap[Value],Unit] = {
          def checkFact(f: TypeChecker2.Fact[Value]): Boolean = {
            f match {
              case TypeChecker2.Fact.Not(negated) => !checkFact(negated)
              case TypeChecker2.Fact.IsType(Value.Null, TypeChecker2.Type.Null) => true
              case TypeChecker2.Fact.IsType(Value.Int(_), TypeChecker2.Type.Int) => true
              case TypeChecker2.Fact.IsType(Value.Product(_), TypeChecker2.Type.Product(_)) => true
              case _ => false
            }
          }
          if (checkFact(fact)) Solutions.single(heap, ()) else Solutions.empty
        }
      }
      override def childValues(value: Value): Solver[Heap[Value], List[Value]] = {
        value match {
          case Value.Product(values) => Solver.const[Heap[Value], List[Value]](values)
          case _ => throw new IllegalArgumentException("Only products have child values: $value")
        }
      }
    }
    val solver = TypeChecker2.check[Value](g, v)
    val solutions = solver.solve(heap)
    assert(solutions.list.size == 1, s"Wrong number of solutions: $solutions")
    solutions.list.head.value.isTrue
  }
  def checkX(v: Value, g: Graph): Boolean = {

    def eval(id: Id, value: Value, stack: List[Check]): Ternary = {
      val nodeCheck = Check(id, value)
      if (stack.contains(nodeCheck)) TUnknown else {
        val node = g.nodes(id)
        def evalChild(child: Id, value: Value): Ternary = eval(child, value, nodeCheck::stack)
        node match {
          case Node.Any => TTrue
          case Node.Void => TFalse
          case Node.Null =>
            value match {
              case Value.Null => TTrue
              case _ => TFalse
            }
          case Node.Int =>
            value match {
              case Value.Int(_) => TTrue
              case _ => TFalse
            }
          case Node.Negation(child) =>
            !evalChild(child, value)
          case Node.Union(children) =>
            children.foldLeft[Ternary](TFalse) {
              case (acc, child) => acc | evalChild(child, value)
            }
          case Node.Product(nodeChildren) =>
            value match {
              case Value.Product(valueChildren) =>
                (nodeChildren zip valueChildren).foldLeft[Ternary](TTrue) {
                  case (acc, (childId, childValue)) => acc & evalChild(childId, childValue)
                }
              case _ => TFalse
            }
        }
      }
    }

    val rootCheck: Ternary = eval(g.root, v, Nil)
    rootCheck.isTrue // Fails if TFalse or TUnknown
  }

  private final case class Check(id: Id, value: Value)

  private sealed trait Expr
  private object Expr {
    final case class Bool(value: Boolean) extends Expr
    final case class And(children: List[Check]) extends Expr
    final case class Or(children: List[Check]) extends Expr
    final case class Not(child: Check) extends Expr
  }

  private sealed trait Expr2
  private object Expr2 {
    final case class Ref(check: Check) extends Expr2
    final case class Tern(value: Ternary) extends Expr2
    final case class And(children: List[Expr2]) extends Expr2
    final case class Or(children: List[Expr2]) extends Expr2
    final case class Not(child: Expr2) extends Expr2
  }
}