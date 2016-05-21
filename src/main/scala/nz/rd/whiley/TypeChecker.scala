package nz.rd.whiley

import scala.annotation.tailrec
import scala.collection.mutable
import nz.rd.whiley.Graph.{ Id, Node }

object TypeChecker {
  
  def check(v: Value, t: Tree): Boolean = check(v, Graph.fromTree(t))
  def check(v: Value, g: Graph): Boolean = check2(v, g)
  def checkXXX(v: Value, g: Graph): Boolean = {

    val checks = mutable.Map.empty[Check, Expr]

    // Get an Expr for the given Check. When this function is first called
    // an Expr will be created automatically and added to the checks map.
    // On subsequent calls the Expr value in the map will be loaded.
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
//      println(checks)
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
          case n@Expr.Not(child) =>
            resolve(child) match {
              case Expr.Bool(b) => Expr.Bool(!b)
              case _ => n // Leave unchanged
            }
        }
        checks(check) = newExpr
      }
    }

    resolve(rootCheck) match {
      case Expr.Bool(b) => b
      case _ => false // Evaluation failed to terminate (not contractive)
    }
  }

  def check2(v: Value, g: Graph): Boolean = {

    def nodeExpr(id: Id, value: Value, stack: List[Check]): Expr2 = {
      val nodeCheck = Check(id, value)
      if (stack.contains(nodeCheck)) Expr2.Ref(nodeCheck) else {
        val node = g.nodes(id)
        node match {
          case Node.Any => Expr2.Bool(true)
          case Node.Void => Expr2.Bool(false)
          case Node.Null =>
            value match {
              case Value.Null => Expr2.Bool(true)
              case _ => Expr2.Bool(false)
            }
          case Node.Int =>
            value match {
              case Value.Int(_) => Expr2.Bool(true)
              case _ => Expr2.Bool(false)
            }
          case Node.Negation(child) =>
            nodeExpr(child, value, nodeCheck :: stack) match {
              case Expr2.Ref(`nodeCheck`) => Expr2.Bool(false)
              case Expr2.Bool(b) => Expr2.Bool(!b)
              case Expr2.Not(e) => e
              case e => e
            }
          case Node.Union(children) =>
            val exprChildren: List[Expr2] = children.flatMap { child: Id =>
              nodeExpr(child, value, nodeCheck :: stack) match {
                case Expr2.Or(es) => es
                case e => e::Nil
              }
            }.filter(_ != Expr2.Ref(nodeCheck))
            println(s"exprChildren: $exprChildren")
            exprChildren match {
              case Nil => Expr2.Bool(true)
              case e::Nil => e
              case es if es.exists(_ == Expr2.Bool(true)) => Expr2.Bool(true)
              case es if es.forall(_ == Expr2.Bool(false)) => Expr2.Bool(false)
              case es => Expr2.Or(es)
            }
          case Node.Record(fields) =>
            val exprChildren: List[Expr2] = fields.map(_._2).flatMap { child: Id =>
              nodeExpr(child, value, nodeCheck :: stack) match {
                case Expr2.And(es) => es
                case e => e::Nil
              }
            }
            exprChildren match {
              case Nil => Expr2.Bool(true)
              case e::Nil => e
              case es if es.forall(_ == Expr2.Bool(true)) => Expr2.Bool(true)
              case es if es.exists(_ == Expr2.Bool(false)) => Expr2.Bool(false)
              case es if es.exists(_ == Expr2.Ref(nodeCheck)) => Expr2.Bool(false)
              case es => Expr2.And(es)
            }
        }
      }
    }

    val rootExpr: Expr2 = nodeExpr(g.root, v, Nil)
    println(s"rootExpr: $rootExpr")
    rootExpr match {
      case Expr2.Bool(b) => b
      case _ => false
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

  private sealed trait Expr2
  private object Expr2 {
    final case class Ref(check: Check) extends Expr2
    final case class Bool(value: Boolean) extends Expr2
    final case class And(children: List[Expr2]) extends Expr2
    final case class Or(children: List[Expr2]) extends Expr2
    final case class Not(child: Expr2) extends Expr2
  }
}