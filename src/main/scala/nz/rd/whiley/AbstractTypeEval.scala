package nz.rd.whiley

final object AbstractTypeEval {

  import Graph.{ Id, Node }

//  final case class AbsValue(id: Int)
//  final case class Context(
//                          absValueCount: Int,
//                          evals: List[Eval]
//                          )
//  final case class Eval(root: Id, v: AbsValue, state: Either[List[Id], Ternary])
//
//
//  def eval(g: Graph, id: Id, v: AbsValue, ctx: Context): Seq[Ternary] = {
//
//  }

  sealed trait Sketch {
    def assume(kind: Kind): Sketch.Assumptions
  }
  object Sketch {
    final case class Assumptions(ifTrue: Option[Sketch], ifFalse: Option[Sketch])

    case object Empty extends Sketch {
      override def assume(kind: Kind): Assumptions = {
        Assumptions(Some(Is(kind)), Some(Exclude(List(kind))))
      }
    }
    case class Is(k: Kind) extends Sketch {
      override def assume(kind: Kind): Assumptions = {
        if (k == kind) {
          // This Sketch already includes the true assumption;
          // the false assumption is not possible
          Assumptions(Some(this), None)
        } else {
          // Contradiction so no valid sketches
          Assumptions(None, None)
        }
      }
    }
    case class Exclude(ks: List[Kind]) extends Sketch {
      override def assume(kind: Kind): Assumptions = {
        if (ks.contains(kind)) {
          // This Sketch already includes the false assumption;
          // the true assumption is not possible
          Assumptions(None, Some(this))
        } else {
          // The true assumption fixes the kind; the false
          // assumption adds a new type to exclude
          Assumptions(Some(Is(kind)), Some(Exclude(kind::ks)))
        }
      }
    }
  }

//  sealed trait Fact
//  object Fact {
//    case class Kind(k: Kind) extends Fact
//    case class Kind(k: Kind) extends Fact
//    case class Not(f: Fact) extends Fact
//  }

  sealed trait Kind
  object Kind {
    case object Int extends Kind
    case object Null extends Kind
  }

  final case class AbsValue(name: String, typeId: Id, sketch: Sketch)
  final case class State()

  final case class Check(id: Id, sketch: Sketch)

  def check(t: Tree): Set[Ternary] = check(Graph.fromTree(t))
  def check(g: Graph): Set[Ternary] = {
    eval(g, Check(g.root, Sketch.Empty), Nil).map(_._2)
  }

  private def evalAssumptions(a: Sketch.Assumptions, ifTrue: Sketch => Set[(Sketch, Ternary)], ifFalse: Sketch => Set[(Sketch, Ternary)]): Set[(Sketch, Ternary)] = {
    def fold(os: Option[Sketch], f: Sketch => Set[(Sketch, Ternary)]): Set[(Sketch, Ternary)] = {
      os match {
        case None => Set.empty
        case Some(s) => f(s)
      }
    }
    fold(a.ifTrue, ifTrue) ++ fold(a.ifFalse, ifFalse)
  }


  private def eval(g: Graph, c: Check, stack: List[Check]): Set[(Sketch, Ternary)] = {
    if (stack.contains(c)) Set((c.sketch, TUnknown)) else {
      def evalChild(id: Id, s: Sketch): Set[(Sketch, Ternary)] = eval(g, Check(id, s), c::stack)
      val n = g.nodes(c.id)
      n match {
        case Node.Any => Set((c.sketch, TTrue))
        case Node.Void => Set((c.sketch, TFalse))
        case Node.Null =>
          evalAssumptions(
            c.sketch.assume(Kind.Null),
            nullSketch => Set((nullSketch, TTrue)),
            notNullSketch => Set((notNullSketch, TFalse))
          )
        case Node.Int =>
          evalAssumptions(
            c.sketch.assume(Kind.Int),
            intSketch => Set((intSketch, TTrue)),
            notIntSketch => Set((notIntSketch, TFalse))
          )
        case Node.Negation(child) =>
          evalChild(child, c.sketch).map { case (sketch, tern) => (sketch, !tern) }
        case Node.Union(children) =>
          children.foldLeft[Set[(Sketch, Ternary)]](Set((c.sketch, TFalse))) {
            case (acc, child) =>
              for {
                (inSketch, inTern) <- acc
                (outSketch, outTern) <- evalChild(child, inSketch)
              } yield (outSketch, inTern | outTern)
          }
      }
    }
  }

//  def eval(g: GraphId, )
}
