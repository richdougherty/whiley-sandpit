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
          case (Node.Null, Value.Null) => Expr.Bool(true)
          case (Node.Null, _) => Expr.Bool(false)
          case (Node.Int, Value.Int(_)) => Expr.Bool(true)
          case (Node.Int, _) => Expr.Bool(false)
          case (Node.Negation(child), v) => Expr.Not(Check(child, v))
          case (Node.Union(children), v) => Expr.Or(children.map((Check(_, v))))
          case (Node.Record(nodeFields), Value.Record(valueFields)) =>
            if (nodeFields.map(_._1) != valueFields.map(_._1)) Expr.Bool(false) else {
              Expr.And((nodeFields zip valueFields).map {
                case ((_, fieldId), (_, fieldValue)) => Check(fieldId, fieldValue)
              })
            }
          case (Node.Record(nodeFields), _) => Expr.Bool(false)
        }
      })
    }

    def flattenChildren(cs: List[Check], exclude: Set[Check])(f: Expr => Option[List[Check]]): List[Check] = cs match {
      case Nil => Nil
      case c::tail if exclude.contains(c) => flattenChildren(tail, exclude)(f)
      case c::tail =>
        f(resolve(c)) match {
          // Flatten in children of nested Or expressions
          case None => c::flattenChildren(tail, exclude + c)(f)
          case Some(grandchildren) => flattenChildren(tail ++ grandchildren, exclude + c)(f)
        }
    }


    val rootCheck = Check(g.root, v)
    resolve(rootCheck)

    Utils.fixpoint {
      checks.toMap
    } {
      val checkList = checks.keys // Fix list of keys known at start of loop
      for (check <- checkList) {
        val newExpr = resolve(check) match {
          case b@Expr.Bool(_) => b // No work to do
          case Expr.Or(children) =>
            val flattenedChildren: List[Check] = flattenChildren(children, Set(check)) {
              case Expr.Or(cs) => Some(cs)
              case _ => None
            }
            flattenedChildren match {
              case Nil => Expr.Bool(false) // Empty Or is false
              case child::Nil => resolve(child) // Singleton And is same as child
              case children if children.exists(resolve(_) == Expr.Bool(true)) => Expr.Bool(true)
              case children => Expr.Or(children)
            }
          case Expr.And(children) =>
            val flattenedChildren: List[Check] = flattenChildren(children, Set(check)) {
              case Expr.And(cs) => Some(cs)
              case _ => None
            }
            flattenedChildren match {
              case Nil => Expr.Bool(true) // Empty And is false
              case child::Nil => resolve(child) // Singleton And is same as child
              case children if children.forall(resolve(_) == Expr.Bool(true)) => Expr.Bool(true)
              case children => Expr.And(children)
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
    final case class And(children: List[Check]) extends Expr
    final case class Or(children: List[Check]) extends Expr
    final case class Not(child: Check) extends Expr
  }
}