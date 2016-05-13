package nz.rd.whiley

import scala.annotation.tailrec
import scala.collection.mutable
import nz.rd.whiley.Graph.{ Id, Node }

object TypeChecker {
  
  def check(v: Value, t: Tree): Boolean = check(v, Graph.fromTree(t))
  def check(v: Value, g: Graph): Boolean = {

    val checks = mutable.Map.empty[Check, Expr]

    def resolve(check: Check): Expr = {
      checks.getOrElseUpdate(check, {
        val n: Node = g.nodes(check.id)
        (n, check.value) match {
          case (Node.Any, _) => Expr.Bool(true)
          case (Node.Void, _) => Expr.Bool(false)
          case (Node.Int, Value.Int(_)) => Expr.Bool(true)
          case (Node.Negation(child), v) => Expr.Not(Check(child, v))
          case (Node.Union(children), v) => Expr.Or(children.map((Check(_, v))))
          case (Node.Record(nodeFields), Value.Record(valueFields)) =>
            if (nodeFields.length != valueFields.length) Expr.Bool(false) else {
              Expr.And((nodeFields zip valueFields).map {
                case ((_, fieldId), (_, fieldValue)) => Check(fieldId, fieldValue)
              })
            }
        }
      })
    }

    val rootCheck = Check(g.root, v)
    resolve(rootCheck)

    Utils.fixpoint {
      checks.toMap
    } {
      val checkList = checks.keys // Fix list of keys known at start of loop
      for (check <- checkList) {
        val newExpr = resolve(check) match {
          // case Expr.Ref(`check`) => Expr.Bool(false) // Self-reference is false
          // case Expr.Ref(other) => resolve(other)
          case b@Expr.Bool(_) => b // No work to do
          case Expr.Or(childrenChecks) =>
            // Evaluate children
            val filteredExpr = childrenChecks.map(c => (c, resolve(c))).foldLeft[Expr](Expr.Or(Nil)) {
              case (Expr.Or(_), (_, Expr.Bool(true))) => Expr.Bool(true) // Short-circuit
              case (u@Expr.Or(_), (_, Expr.Bool(false))) => u // Ignore false
              case (u@Expr.Or(_), (_, Expr.Ref(`check`))) => u // Ignore self-reference
              case (Expr.Or(children), (_, Expr.Or(grandchildren))) => Expr.Or(children ++ grandchildren)
              case (Expr.Or(children), (child, _)) => Expr.Or(child::children)
            }
            // Rewrite pathological forms of union
            filteredExpr match {
              case Expr.Or(Nil) => Expr.Bool(false) // Empty union is false
              case Expr.Or(child::Nil) => resolve(child) // Singleton union is same as child (we know child != `check`)
              case e => e
            }
          case Expr.Not(`check`) => Expr.Bool(false) // Self-reference is false
          case Expr.Not(child) =>
            resolve(child) match {
              case Expr.Bool(b) => Expr.Bool(!b)
              case other => other
            }
        }
        checks(check) = newExpr
      }
    }

    resolve(rootCheck) match {
      case Expr.Bool(b) => b
      case _ => false // Evaluation failed to terminate (probably contractive type?)
    }
  }

  private final case class Check(id: Id, value: Value)

  private sealed trait Expr
  private object Expr {
    final case class Bool(value: Boolean) extends Expr
    final case class Ref(check: Check) extends Expr
    final case class And(children: List[Check]) extends Expr
    final case class Or(children: List[Check]) extends Expr
    final case class Not(child: Check) extends Expr
  }
}