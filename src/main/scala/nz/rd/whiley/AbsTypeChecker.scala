package nz.rd.whiley

import nz.rd.whiley.AbstractTypeEval.Check
import nz.rd.whiley.Graph.Id
import nz.rd.whiley.solve.{Solution, Solutions, Solver}

import scala.annotation.tailrec

class AbsTypeChecker extends BaseTypeChecker {

//  sealed trait AbsVal
//  final case object RootVal extends AbsVal
//  final case class ProductVal(parent: AbsVal, index: Int) extends AbsVal

//  sealed trait Expr
//  object Expr {
//    final case class IsKind(value: V, k: Kind) extends Expr
//    final case class Const(value: Ternary) extends Expr
//    final case class And(children: List[Expr]) extends Expr
//    final case class Or(children: List[Expr]) extends Expr
//    final case class Not(child: Expr) extends Expr
//  }

//  class Disj(val min: Ternary, val conjs: Set[Conj]) {
//    require(min != TTrue || conjs.isEmpty)
//
//    def max: Ternary = if (conjs.isEmpty) min else TTrue
//
////    def unary_!(): Disj = {
////
////    }
//
//    def |(conj: Conj): Disj = {
//      if (min == TTrue || conj.max == TFalse) {
//        this
//      } else if (conjs.exists(c => c.max == (c.max | conj.max) && c.terms.subsetOf(conj.terms))) {
//        this
//      } else {
//        new Disj(min | conj.min, conjs + conj)
//      }
//    }
//    def |(that: Disj): Disj = {
//      that.conjs.foldLeft(new Disj(this.min | that.min, this.conjs)) {  case (d, c) => d | c }
//    }
//    def possibleValues: Set[Ternary] = {
//      val start: Set[Ternary] = min match {
//        case TTrue => Set(TTrue)
//        case TUnknown => Set(TTrue, TUnknown)
//        case TTrue => Set(TTrue, TUnknown, TFalse)
//      }
//      conjs.foldLeft(start) {
//        case (currSet, c) =>
//          for {
//            a <- currSet
//            b <- c.possibleValues
//          } yield a | b
//      }
//    }
//  }
//
//  object Disj {
//    val Empty: Disj = False
//    val True: Disj = new Disj(TTrue, Set.empty)
//    val Unknown: Disj = new Disj(TUnknown, Set.empty)
//    val False: Disj = new Disj(TFalse, Set.empty)
//  }
//
//  class Conj(val max: Ternary, val terms: Set[Term]) {
//    require(max != TFalse || terms.isEmpty)
//
//    def min: Ternary = if (terms.isEmpty) max else TFalse
//
//    def &(ternary: Ternary): Conj = ternary match {
//      case TTrue => this
//      case TUnknown if max == TFalse => this
//      case TUnknown => new Conj(TUnknown, terms)
//      case TFalse => Conj.False
//    }
//    def &(term: Term): Conj = {
//      if (max == TFalse) {
//        this
//      } else if (terms.exists(t => t.pos != term.pos && t.kind == term.kind)) {
//        Conj.False
//      } else if (term.pos && terms.exists(t => t.pos && t.kind == term.kind)) {
//        Conj.False
//      } else if (terms.contains(term)) {
//        this
//      } else {
//        new Conj(max, terms + term)
//      }
//    }
//    def &(that: Conj): Conj = {
//      that.terms.foldLeft(new Conj(this.max & that.max, this.terms)) { case (c, t) => c & t }
//    }
//    def possibleValues: Set[Ternary] = {
//      val start: Set[Ternary] = max match {
//        case TTrue => Set(TTrue, TUnknown, TFalse)
//        case TUnknown => Set(TUnknown, TFalse)
//        case TFalse => Set(TFalse)
//      }
////      terms.foldLeft(start) {
////        case (currSet, term) =>
////          for {
////            a <- currSet
////            b <- term.possibleValues
////          } yield a & b
////      }
//      // Optimization for above loop since terms can't evaluate to TUnknown at the current time
//      if (terms.isEmpty) { start } else { start - TUnknown }
//    }
//  }
//
//  object Conj {
//    val True: Conj = new Conj(TTrue, Set.empty)
//    val Unknown: Conj = new Conj(TUnknown, Set.empty)
//    val False: Conj = new Conj(TFalse, Set.empty)
//    val Empty: Conj = True
//    def apply(terms: Term*): Conj = terms.foldLeft(Conj.Empty) { case (c, t) => c & t }
//  }
//
//  final case class Term(pos: Boolean, kind: Kind) {
//    def unary_!(): Term = Term(!pos, kind)
//    def possibleValues: Set[Ternary] = Set(TTrue, TFalse)
//  }
//
//  sealed trait Kind
//  object Kind {
//    case object Null extends Kind
//    case object Int extends Kind
//    case class Product(size: Int) extends Kind
//  }

  import DNF.Disj

  @tailrec
  final def evalExpr(expr: Disj): Set[Ternary] = expr match {
    case Disj.Const(value) => Set(value)
    case Disj.And(children) => children.map(evalDisj(_)).foldLeft[Ternary](TTrue) { case (acc, t) => acc & t }
  }

  case class ExtraContext(compoundTypeChecks: Set[Graph.Id], nestedResultAssumptions: Set[(Graph.Id, Ternary)])

  type V = AbsVal
  type R = Disj
  type X = ExtraContext

  override protected val RTrue: Disj = Disj.True
  override protected val RFalse: Disj = Disj.False
  override protected val RUnknown: Disj = Disj.Unknown
  override protected def resultNegation(r: Disj): Disj = !r
  override protected def resultDisjunction(r1: Disj, r2: Disj): Disj = Disj.Or(List(r1, r2))
  override protected def resultConjunction(r1: Disj, r2: Disj): Disj = Disj.And(List(r1, r2))
  override protected def resultCondition[C](r: Disj, ifTrue: =>Solver[C,R], ifFalse: =>Solver[C,R]): Solver[C,R] = {
    ifTrue.map(trueResult => Disj.And(List(r, trueResult))) ++ ifFalse.map(falseResult => Disj.And(List(Disj.Not(r), falseResult)))
  }

  def check(g: Graph): List[Disj] = {
    checkSolutions(g, RootVal)
  }

  override protected def initialExtraContext: ExtraContext = ExtraContext(Set.empty, Set.empty)
  override protected def isProductKind(size: Int, v: AbsVal): Disj = Disj.IsKind(v, Kind.Product(size))
  override protected def isIntKind(v: AbsVal): Disj = Disj.IsKind(v, Kind.Int)
  override protected def isNullKind(v: AbsVal): Disj = Disj.IsKind(v, Kind.Null)
  override protected def checkCompoundType(
      compoundValue: AbsVal,
      typeId: Id,
      typeNode: Graph.Node,
      checkChildren: (List[AbsVal]) => Solver[ExtraContext, Disj]): Solver[ExtraContext, Disj] = {

    for {
      initialExtraContext <- Solver.getContext[ExtraContext]
      r <- if (initialExtraContext.compoundTypeChecks.contains(typeId)) {
        Solver { x: ExtraContext =>
          def assumedSolution(result: Ternary): Solution[X,Disj] = {
            Solution(x.copy(nestedResultAssumptions = x.nestedResultAssumptions + (typeId -> TFalse)), Disj.Const(result))
          }
          Solutions(List(
            assumedSolution(TTrue),
            //assumedSolution(TFalse), // TODO: Probably not needed
            assumedSolution(TUnknown)
          ))
        }
      } else {
        Solver { x: ExtraContext =>
          val childValues: List[AbsVal] = typeNode match {
            case Graph.Node.Product(childTypeIds) => childTypeIds.indices.map(ProductVal(compoundValue, _)).toList
            case _ => throw new IllegalArgumentException(s"Can't get children of non-compound type: $compoundValue, $typeId, $typeNode")
          }
          val childSolutions = checkChildren(childValues).solve(x.copy(compoundTypeChecks = x.compoundTypeChecks + typeId))
          val assumedSolutions = childSolutions.foldLeft((Set.empty[Disj], Map.empty[Ternary,Set[Disj]])) {
            case (Solution(x2: ExtraContext, expr), (known, assumed)) => ???
          }
        }
        for {
          _ <- Solver.mapContext[ExtraContext](x => )
          resultsFromAssumptions <- checkChildren(childValues)

          _ <- Solver.mapContext[ExtraContext](x => x.copy(compoundTypeChecks = x.compoundTypeChecks - typeId))
        } yield r
      }
    } yield r
  }
}

