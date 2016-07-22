package nz.rd.whiley

import nz.rd.whiley.solve.{Solutions, Solver}

trait BaseTypeChecker {

  type V
  type R
  type X

  protected def initialExtraContext: X

  protected val RFalse: R
  protected val RTrue: R
  protected val RUnknown: R
  protected def resultNegation(r: R): R
  protected def resultConjunction(r1: R, r2: R): R
  protected def resultDisjunction(r1: R, r2: R): R
  protected def resultCondition[C](r: R, ifTrue: => Solver[C,R], ifFalse: => Solver[C,R]): Solver[C,R]

  protected def isNullKind(v: V): R
  protected def isIntKind(v: V): R
  protected def isProductKind(size: Int, v: V): R
  protected def checkCompoundType(compoundValue: V, typeId: Graph.Id, typeNode: Graph.Node, checkChildren: List[V] => Solver[X,R]): Solver[X,R]

  def checkSolutions(graph: Graph, value: V): List[R] = {
    val ctx = Context(checks = Set.empty, extraContext = initialExtraContext)
    val solutions = check0(graph, value, graph.root).solve(ctx)
    solutions.results
  }

  private case class Context(checks: Set[(V, Graph.Id)], extraContext: X)

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

  private def const(r: R): Solver[Context,R] = {
    Solver.const[Context,R](r)
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
              case Graph.Node.Any => const(RTrue)
              case Graph.Node.Void => const(RFalse)
              case Graph.Node.Null => resultCondition(
                isNullKind(value),
                const(RTrue),
                const(RFalse)
              )
              case Graph.Node.Int => resultCondition(
                isIntKind(value),
                const(RTrue),
                const(RFalse)
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
              case Graph.Node.Intersection(childTypeIds) =>
                Solver.foldLeft[Context, Graph.Id, R](childTypeIds, RTrue) {
                  case (acc, childTypeId) =>
                    for {
                      childCheck <- check0(graph, value, childTypeId)
                    } yield resultConjunction(acc, childCheck)
                }
              case Graph.Node.Product(childTypeIds) => resultCondition(
                isProductKind(childTypeIds.size, value),
                for {
                  capturedContext <- Solver.getContext[Context]
                  r <- Solver.wrapContext[Context,X,R](
                    c => c.extraContext,
                    x => capturedContext.copy(extraContext = x)
                  ) {
                    println(s"Calling checkCompoundType for $value, $typeId, $typeNode")
                    checkCompoundType(value, typeId, typeNode, { childValues: List[V] =>
                      println(s"checkCompoundType/checkChildren called with $childValues")
                      assert(childValues.size == childTypeIds.size) // Should be enforced by isProductKind condition
                      Solver.wrapContext[X, Context, R](
                        x => capturedContext.copy(extraContext = x),
                        c => c.extraContext
                      ) {
                        Solver.foldLeft[Context, (Graph.Id, V), R]((childTypeIds zip childValues), RTrue) {
                          case (acc, (childTypeId, childValue)) =>
                            for {
                              childCheck <- check0(graph, childValue, childTypeId)
                            } yield resultConjunction(acc, childCheck)
                        }
                      }
                    })
                  }
                } yield r,
                const(RFalse)
              )
            }
          }
          _ <- leaveCheckFrame(value, typeId)
        } yield typeCheck
      }
    } yield {
      println(s"Result for $value against type $typeId  (${graph.nodes(typeId)}): $loopCheck")
      loopCheck
    }
  }

}