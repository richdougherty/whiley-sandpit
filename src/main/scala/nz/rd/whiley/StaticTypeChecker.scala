package nz.rd.whiley

import nz.rd.whiley.Graph.Id
import nz.rd.whiley.solve.{Solution, Solutions, Solver}

import scala.annotation.tailrec

object StaticTypeChecker extends BaseTypeChecker {

  import DNF._

  case class ExtraContext(compoundTypeChecks: Set[Graph.Id], nestedResultAssumptions: Map[Graph.Id, Set[Ternary]])

  type V = AbsVal
  type R = Disj
  type X = ExtraContext

  override protected val RTrue: Disj = Disj.True
  override protected val RFalse: Disj = Disj.False
  override protected val RUnknown: Disj = Disj.Unknown
  override protected def resultNegation(r: Disj): Disj = !r
  override protected def resultDisjunction(r1: Disj, r2: Disj): Disj = r1 | r2
  override protected def resultConjunction(r1: Disj, r2: Disj): Disj = r1 & r2
  override protected def resultCondition[C](r: Disj, ifTrue: =>Solver[C,R], ifFalse: =>Solver[C,R]): Solver[C,R] = {
    for {
      t <- ifTrue
      f <- ifFalse
    } yield (r & t) | (!r & f)
  }

  def check(t: Tree): List[Disj] = check(Graph.fromTree(t))
  def check(g: Graph): List[Disj] = {
    checkSolutions(g, RootVal)
  }

  override protected def initialExtraContext: ExtraContext = ExtraContext(Set.empty, Map.empty)
  override protected def isProductKind(size: Int, v: AbsVal): Disj = Disj(Term.isKind(v, Kind.Product(size)))
  override protected def isIntKind(v: AbsVal): Disj = Disj(Term.isKind(v, Kind.Int))
  override protected def isNullKind(v: AbsVal): Disj = Disj(Term.isKind(v, Kind.Null))
  override protected def checkCompoundType(
      compoundValue: AbsVal,
      typeId: Id,
      typeNode: Graph.Node,
      checkChildren: (List[AbsVal]) => Solver[ExtraContext, Disj]): Solver[ExtraContext, Disj] = {
    for {
      initialExtraContext <- Solver.getContext[ExtraContext]

      // Check whether we're already type checking this compound type.

      r <- if (initialExtraContext.compoundTypeChecks.contains(typeId)) {

        // We're already type checking this compound type. Let's make some assumptions for the result of typechecking
        // the recursive call. If we assume that the nested type check result is true, false and unknown, then what
        // does that imply about the parent type check?

        Solver { x: ExtraContext =>
          def assumedSolution(result: Ternary): Solution[X,Disj] = {
            println(s"Assuming $result")
            // Record the fact that required assumption. The map may already have this assumption if there is more
            // than one nested typecheck. It's OK to add the same assumed value twice.
            val existingAssumptionsForType: Set[Ternary] = x.nestedResultAssumptions.getOrElse(typeId, Set.empty)
            val newAssumptionsForType = existingAssumptionsForType + result
            Solution(
              x.copy(nestedResultAssumptions = x.nestedResultAssumptions + (typeId -> newAssumptionsForType)),
              Disj(result)
            )
          }
          Solutions(List(
            assumedSolution(TTrue),
            assumedSolution(TFalse),
            assumedSolution(TUnknown)
          ))
        }
      } else {

        // We're not currently type checking this compound type. Type check all its child values. If one of the children
        // requires a recursive type check of this type then we don't actually want to perform a full type check on this
        // type again because that will cause an infinite loop. Instead, we will terminate the loop by making an
        // assumption about the result of the nested type check. This assumption will have an effect on the type of the
        // parent type.

        Solver[ExtraContext, Disj] { x: ExtraContext =>

          // FIXME: Handle other assumptions in the context.
          // FIXME: Remove assumptions from the context as we handle them.

          // Create new abstract values for each child.
          val childValues: List[AbsVal] = typeNode match {
            case Graph.Node.Product(childTypeIds) => childTypeIds.indices.map(ProductVal(compoundValue, _)).toList
            case _ => throw new IllegalArgumentException(s"Can't get children of non-compound type: $compoundValue, $typeId, $typeNode")
          }

          // Run the type checker on the children.
          val childSolutions: Solutions[ExtraContext, Disj] =
          checkChildren(childValues).solve(x.copy(compoundTypeChecks = x.compoundTypeChecks + typeId))

          println(s"Solutions for children: $childSolutions")

          // The children m
          val assumptionMap: Map[Set[Ternary], (Set[Ternary], List[Solution[ExtraContext,Disj]])] =
              childSolutions.list.foldLeft[Map[Set[Ternary], (Set[Ternary], List[Solution[ExtraContext,Disj]])]](Map.empty) {
            case (acc, solution@Solution(x2: ExtraContext, disj)) =>
              // Get the set of assumptions that were made.
              val assumptions: Set[Ternary] = x2.nestedResultAssumptions.getOrElse(typeId, Set.empty)
              val existingAccEntry: (Set[Ternary], List[Solution[ExtraContext,Disj]]) =
                acc.getOrElse(assumptions, (Set.empty[Ternary], Nil))

              // Get the possible ternary values for the Disj expression. The Disj may have a single solution, e.g.
              // TFalse, or it may have a range of values, e.g. {TFalse, TUnknown, TTrue}.
              val solutionValues: Set[Ternary] = disj.possibleValues
              println(s"Recording child solution $solution with assumptions $assumptions and possible values: $solutionValues")
              val newValues: (Set[Ternary], List[Solution[ExtraContext,Disj]]) =
                (existingAccEntry._1 ++ solutionValues, solution::existingAccEntry._2)
              acc + (assumptions -> newValues)
          }

          println(s"Assumption map for children: $assumptionMap")

          // Now walk through the assumptions building a list of

          val all: List[Set[Ternary]] = (for (i <- (0 until 32)) yield {
            def setForBit(b: Int, t: Ternary): Set[Ternary] = {
              if ((i & (2 << b)) != 0) {
                Set[Ternary](t)
              } else Set.empty[Ternary]
            }
            setForBit(0, TTrue) ++ setForBit(1, TFalse) ++ setForBit(3, TUnknown)
          }).toList
          println(s"All ternary combinations: $all")

          @tailrec
          def loop(workRemaining: Seq[Set[Ternary]], allWork: Set[Set[Ternary]], possible: Set[Ternary]): Set[Ternary] = {
            if (possible.size == 3) {
              // Optimization: All ternary values already possible, end early
              possible
            } else {
              // See if we can find more possible values
              workRemaining match {
                case Nil => possible
                case workHead :: workTail =>
                  assumptionMap.get(workHead) match {
                    case None => loop(workTail, allWork, possible)
                    case Some((values, _)) =>
                      val newPossible = possible ++ values
                      if (newPossible == possible) {
                        // No change
                        loop(workTail, allWork, possible)
                      } else {
                        // New possible value
                        val newWork = all.filter { ts: Set[Ternary] =>
                          ts.subsetOf(newPossible) && !allWork.contains(ts)
                        }
                        loop(newWork ++ workTail, allWork ++ newWork, newPossible)
                      }
                  }
              }
            }
          }

          val possible: Set[Ternary] = loop(List(Set.empty, Set(TFalse)), Set(Set(TFalse)), Set(TFalse))
          println(s"Possible values for children: $possible")
          val solutions: List[Solution[ExtraContext, Disj]] = all.filter(_.subsetOf(possible)).flatMap { p: Set[Ternary] =>
            assumptionMap.getOrElse(p, (Set.empty, Nil))._2
          }
          println(s"Possible solutions for children: $solutions")
          Solutions(solutions)
        }
      }
    } yield r
  }
}

