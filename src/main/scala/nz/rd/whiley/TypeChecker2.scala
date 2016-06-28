package nz.rd.whiley

import nz.rd.whiley.solve.{Solutions, Solver}

object TypeChecker2 {

//  def check(v: Value, t: Tree): Boolean = check(v, Graph.fromTree(t))
//
//  def check(v: Value, g: Graph): Boolean = {
//
//
//  }

  sealed trait Fact[V] {
    def unary_!(): Fact[V] = Fact.Not(this)
  }
  object Fact {
    case class Not[V](negated: Fact[V]) extends Fact[V] {
      require(!negated.isInstanceOf[Not[V]])
      override def unary_!(): Fact[V] = negated
    }
    case class IsType[V](value: V, k: Type) extends Fact[V]
  }

  sealed trait Type
  object Type {
    case object Null extends Type
    case object Int extends Type
    case class Product(typeId: Graph.Id) extends Type
  }

  trait Heap[V] {
    def assert(fact: Fact[V]): Solver[Heap[V], Unit]
    def childValues(value: V): Solver[Heap[V], List[V]]
  }

  def check[V](graph: Graph, value: V): Solver[Heap[V],Ternary] = {
    check0(graph, value, graph.root).changeContext(Context(_, Set.empty), _.heap)
  }

  private case class Context[V](heap: Heap[V], checks: Set[(V, Graph.Id)])

  private def enterCheckFrame[V](value: V, typeId: Graph.Id) = Solver[Context[V],Unit] { c: Context[V] =>
    assert(!c.checks.contains((value -> typeId)))
    val c1 = c.copy(checks = c.checks + (value -> typeId))
    Solutions.single(c1, ())
  }
  private def inCheckFrame[V](value: V, typeId: Graph.Id): Solver[Context[V], Boolean] = {
    for {
      c <- Solver.getContext[Context[V]]
    } yield c.checks.contains(value -> typeId)
  }
  private def leaveCheckFrame[V](value: V, typeId: Graph.Id) = Solver[Context[V],Unit] { c: Context[V] =>
    assert(c.checks.contains((value -> typeId)))
    val c1 = c.copy(checks = c.checks - (value -> typeId))
    Solutions.single(c1, ())
  }

  private def assertFact[V](fact: Fact[V]): Solver[Context[V],Unit] = {
    for {
      initialContext <- Solver.getContext[Context[V]]
      _ <- initialContext.heap.assert(fact).changeContext[Context[V]](_.heap, newHeap => initialContext.copy(heap = newHeap))
    } yield ()
  }
  private def childValues[V](value: V): Solver[Context[V],List[V]] = {
    for {
      initialContext <- Solver.getContext[Context[V]]
      values <- initialContext.heap.childValues(value).changeContext[Context[V]](_.heap, newHeap => initialContext.copy(heap = newHeap))
    } yield values
  }

  private def branch[V,A](
      fact: Fact[V],
      ifTrue: => Solver[Context[V],A],
      ifFalse: => Solver[Context[V],A]): Solver[Context[V],A] = {
    assertFact(fact).flatMap(_ => ifTrue) ++ assertFact(!fact).flatMap(_ => ifFalse)
  }

  private def check0[V](graph: Graph, value: V, typeId: Graph.Id): Solver[Context[V],Ternary] = {
    for {
      alreadyChecking <- inCheckFrame(value, typeId)
      loopCheck <- if (alreadyChecking) Solver.const[Context[V],Ternary](TUnknown) else {
        for {
          _ <- enterCheckFrame(value, typeId)
          typeCheck <- {
            val typeNode = graph.nodes(typeId)
            typeNode match {
              case Graph.Node.Any => Solver.const[Context[V],Ternary](TTrue)
              case Graph.Node.Void => Solver.const[Context[V],Ternary](TFalse)
              case Graph.Node.Null => branch(
                Fact.IsType(value, Type.Null),
                Solver.const[Context[V],Ternary](TTrue),
                Solver.const[Context[V],Ternary](TFalse)
              )
              case Graph.Node.Int => branch(
                Fact.IsType(value, Type.Int),
                Solver.const[Context[V],Ternary](TTrue),
                Solver.const[Context[V],Ternary](TFalse)
              )
              case Graph.Node.Negation(negatedTypeId) =>
                for {
                  t <- check0(graph, value, negatedTypeId)
                } yield !t
              case Graph.Node.Union(childTypeIds) =>
                Solver.foldLeft[Context[V], Graph.Id, Ternary](childTypeIds, TFalse) {
                  case (acc, childTypeId) =>
                    for {
                      childCheck <- check0(graph, value, childTypeId)
                    } yield acc | childCheck
                }
              case Graph.Node.Product(childTypeIds) =>
                branch(
                  Fact.IsType(value, Type.Product(typeId)),
                  for {
                    childValues <- childValues(value) // FIXME: Check product type size?
                    t <- Solver.foldLeft[Context[V], Graph.Id, Ternary](childTypeIds, TTrue) {
                      case (acc, childTypeId) =>
                        for {
                          childCheck <- check0(graph, value, childTypeId)
                        } yield acc & childCheck
                    }
                  } yield t,
                  Solver.const[Context[V],Ternary](TFalse)
                )
            }
          }
          _ <- leaveCheckFrame(value, typeId)
        } yield typeCheck
      }
    } yield loopCheck
  }

}