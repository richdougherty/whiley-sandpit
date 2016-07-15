package nz.rd.whiley

import nz.rd.whiley.Graph.{Id, Node}
import nz.rd.whiley.solve.{Solutions, Solver}

object TypeChecker extends BaseTypeChecker {

  override type V = Value
  override type R = Ternary
  override type X = Unit

  override protected def initialExtraContext: X = ()

  override protected def isNullKind(v: V): R = v match {
    case Value.Null => TTrue
    case _ => TFalse
  }
  override protected def isIntKind(v: V): R = v match {
    case Value.Int(_) => TTrue
    case _ => TFalse
  }
  override protected def isProductKind(size: Int, v: V): R = v match {
    case Value.Product(vs) if vs.length == size => TTrue
    case _ => TFalse
  }
  override protected def checkCompoundType(compoundValue: V, typeId: Graph.Id, typeNode: Graph.Node, checkChildren: List[V] => Solver[X,R]): Solver[X,R] = {
    compoundValue match {
      case Value.Product(vs) => checkChildren(vs)
      case _ => throw new IllegalArgumentException(s"Called on non-compound value: $compoundValue")
    }
  }

  def check(v: Value, t: Tree): Boolean = check(v, Graph.fromTree(t))
  def check(v: Value, g: Graph): Boolean = {
    val solutions = checkSolutions(g, v)
    assert(solutions.size == 1, s"Wrong number of solutions: $solutions")
    solutions.head.isTrue
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

  override protected def resultNegation(r: Ternary): Ternary = !r
  override protected def resultDisjunction(r1: Ternary, r2: Ternary): Ternary = r1 | r2
  override protected def resultConjunction(r1: Ternary, r2: Ternary): Ternary = r1 & r2
  override protected def resultCondition[C](r: R, ifTrue: => Solver[C,R], ifFalse: => Solver[C,R]): Solver[C,R] = {
    if (r.isTrue) ifTrue else ifFalse
  }
  override protected val RFalse: Ternary = TFalse
  override protected val RTrue: Ternary = TTrue
  override protected val RUnknown: Ternary = TUnknown
}