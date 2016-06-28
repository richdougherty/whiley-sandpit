package nz.rd.whiley

import nz.rd.whiley.solve.{Solutions, Solver}

trait TypeChecker2[V,R] {

  sealed trait Fact {
    def unary_!(): Fact = Fact.Not(this)
  }
  object Fact {
    case class Not(negated: Fact) extends Fact {
      require(!negated.isInstanceOf[Not])
      override def unary_!(): Fact = negated
    }
    case class IsType(value: V, k: Type) extends Fact
  }

  sealed trait Type
  object Type {
    case object Null extends Type
    case object Int extends Type
    case class Product(typeId: Graph.Id) extends Type
  }

  protected def resultNegation(r: R): R
  protected def resultConjunction(r1: R, r2: R): R
  protected def resultDisjunction(r1: R, r2: R): R
  protected val RFalse: R
  protected val RTrue: R
  protected val RUnknown: R

  trait Heap {
    def assert(fact: Fact): Solver[Heap, Unit]
    def childValues(value: V): Solver[Heap, List[V]]
  }

  def checkSolutions(graph: Graph, value: V): Solver[Heap,R] = {
    check0(graph, value, graph.root).changeContext(Context(_, Set.empty), _.heap)
  }

  private case class Context(heap: Heap, checks: Set[(V, Graph.Id)])

  private def enterCheckFrame(value: V, typeId: Graph.Id) = Solver[Context,Unit] { c: Context =>
    assert(!c.checks.contains((value -> typeId)))
    val c1 = c.copy(checks = c.checks + (value -> typeId))
    Solutions.single(c1, ())
  }
  private def inCheckFrame(value: V, typeId: Graph.Id): Solver[Context, Boolean] = {
    for {
      c <- Solver.getContext[Context]
    } yield c.checks.contains(value -> typeId)
  }
  private def leaveCheckFrame(value: V, typeId: Graph.Id) = Solver[Context,Unit] { c: Context =>
    assert(c.checks.contains((value -> typeId)))
    val c1 = c.copy(checks = c.checks - (value -> typeId))
    Solutions.single(c1, ())
  }

  private def assertFact(fact: Fact): Solver[Context,Unit] = {
    for {
      initialContext <- Solver.getContext[Context]
      _ <- initialContext.heap.assert(fact).changeContext[Context](_.heap, newHeap => initialContext.copy(heap = newHeap))
    } yield ()
  }
  private def childValues(value: V): Solver[Context,List[V]] = {
    for {
      initialContext <- Solver.getContext[Context]
      values <- initialContext.heap.childValues(value).changeContext[Context](_.heap, newHeap => initialContext.copy(heap = newHeap))
    } yield values
  }

  private def branch[A](
      fact: Fact,
      ifTrue: => Solver[Context,A],
      ifFalse: => Solver[Context,A]): Solver[Context,A] = {
    assertFact(fact).flatMap(_ => ifTrue) ++ assertFact(!fact).flatMap(_ => ifFalse)
  }

  private def check0(graph: Graph, value: V, typeId: Graph.Id): Solver[Context,R] = {
    for {
      alreadyChecking <- inCheckFrame(value, typeId)
      loopCheck <- if (alreadyChecking) Solver.const[Context,R](RUnknown) else {
        for {
          _ <- enterCheckFrame(value, typeId)
          typeCheck <- {
            val typeNode = graph.nodes(typeId)
            typeNode match {
              case Graph.Node.Any => Solver.const[Context,R](RTrue)
              case Graph.Node.Void => Solver.const[Context,R](RFalse)
              case Graph.Node.Null => branch(
                Fact.IsType(value, Type.Null),
                Solver.const[Context,R](RTrue),
                Solver.const[Context,R](RFalse)
              )
              case Graph.Node.Int => branch(
                Fact.IsType(value, Type.Int),
                Solver.const[Context,R](RTrue),
                Solver.const[Context,R](RFalse)
              )
              case Graph.Node.Negation(negatedTypeId) =>
                for {
                  t <- check0(graph, value, negatedTypeId)
                } yield resultNegation(t)
              case Graph.Node.Union(childTypeIds) =>
                Solver.foldLeft[Context, Graph.Id, R](childTypeIds, RFalse) {
                  case (acc, childTypeId) =>
                    for {
                      childCheck <- check0(graph, value, childTypeId)
                    } yield resultDisjunction(acc, childCheck)
                }
              case Graph.Node.Product(childTypeIds) =>
                branch(
                  Fact.IsType(value, Type.Product(typeId)),
                  for {
                    childValues <- childValues(value) // FIXME: Check product type size?
                    t <- Solver.foldLeft[Context, (Graph.Id, V), R]((childTypeIds zip childValues), RTrue) {
                      case (acc, (childTypeId, childValue)) =>
                        for {
                          childCheck <- check0(graph, childValue, childTypeId)
                        } yield resultConjunction(acc, childCheck)
                    }
                  } yield t,
                  Solver.const[Context,R](RFalse)
                )
            }
          }
          _ <- leaveCheckFrame(value, typeId)
        } yield typeCheck
      }
    } yield loopCheck
  }

}