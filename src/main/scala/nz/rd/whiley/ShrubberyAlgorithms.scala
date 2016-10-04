package nz.rd.whiley

import scala.collection.mutable
import Shrub.Ref

import scala.annotation.tailrec

final object ShrubberyAlgorithms {

  def reachable(root: Shrub.Ref): Set[Shrub.Ref] = {
    val reached = mutable.Set[Ref]()
    def reachRef(r: Ref): Unit = {
      if (reached.contains(r)) () else {
        reached += r
        reachShrub(r.get)
      }
    }
    def reachShrub(s: Shrub): Unit = s match {
      case Shrub.Any => ()
      case Shrub.Void => ()
      case Shrub.Null => ()
      case Shrub.Int => ()
      case Shrub.Negation(child) => reachShrub(child)
      case Shrub.Union(children) => children.foreach(reachShrub(_))
      case Shrub.Intersection(children) => children.foreach(reachShrub(_))
      case Shrub.Product(children) => children.foreach(reachRef(_))
    }
    reachRef(root)
    reached.toSet
  }

  def mergeIdentical(root: Shrub.Ref): Unit = {
    val index = mutable.Map.empty[Shrub, Ref]
    for (ref <- reachable(root)) {
      val shrub = ref.get
      index.get(shrub) match {
        case Some(existingRef) =>
          // We found an identical shrub, record the ids for later updating
          ref.link(existingRef)
        case None =>
          // New shrub for the index
          index += (shrub -> ref)
      }
    }
  }

  def simplifyIdentities(ref: Shrub.Ref): Unit = {
    val s: Shrub = simplifyIdentities(ref.get)
    ref.set(s)
  }

  def simplifyIdentities(s: Shrub): Shrub = s match {
    case Shrub.Any => Shrub.Any
    case Shrub.Void => Shrub.Void
    case Shrub.Null => Shrub.Null
    case Shrub.Int => Shrub.Int
    case Shrub.Negation(Shrub.Negation((c))) => c
    case Shrub.Union(children) =>
      children.map(simplifyIdentities(_)).flatMap {
        case Shrub.Void => Nil
        case Shrub.Union(children) => children
        case c => c::Nil
      }.distinct match {
        case Nil => Shrub.Void
        case List(c) => c
        case cs if cs.contains(Shrub.Any) => Shrub.Any
        case cs => Shrub.Union(cs)
      }
    case Shrub.Intersection(children) =>
      children.map(simplifyIdentities(_)).flatMap {
        case Shrub.Any => Nil
        case Shrub.Intersection(children) => children
        case c => c::Nil
      }.distinct match {
        case Nil => Shrub.Any
        case List(c) => c
        case cs if cs.contains(Shrub.Void) => Shrub.Void
        case cs => Shrub.Intersection(cs)
      }
    case p@Shrub.Product(_) => p
    case _ => s
  }

  /**
   * Conservatively convert uninhabitable recursive types into
   * the void type. This converts types like `µX.<X>` to void.
   * The conversion is conservative; especially in the presence
   * of negation it is difficult to be precise. To get good
   * results you may need to convert to disjunctive normal form
   * (DNF) first.
   */
  def removeNonterminatingLoops(root: Shrub.Ref): Unit = {
    val shrubTermination = mutable.Map[Ref, Boolean]()

    def mayTerminate(s: Shrub): Boolean = s match {
      case Shrub.Any => true
      case Shrub.Void => false
      case Shrub.Null => true
      case Shrub.Int => true
      case Shrub.Negation(Shrub.Any) => false
      case Shrub.Negation(Shrub.Void) => true
      case Shrub.Negation(Shrub.Int) => true
      case Shrub.Negation(Shrub.Null) => true
      case Shrub.Negation(Shrub.Product(_)) => true
      case Shrub.Union(children) => children.map(mayTerminate).foldLeft(false)(_ | _)
      case Shrub.Intersection(children) => children.map(mayTerminate).foldLeft(true)(_ & _)
      case Shrub.Product(refs) => refs.map(shrubTermination.getOrElse(_, false)).foldLeft(true)(_ & _)
      case _ => true
    }

    val refs = reachable(root)

    Utils.fixpoint(shrubTermination.toMap) {
      for (r <- refs) {
        shrubTermination(r) = mayTerminate(r.get)
      }
    }

    for (r <- refs) {
      if (!shrubTermination(r)) {
        r.set(Shrub.Void)
      }
    }
  }

  def convertToDNF(root: Ref): Unit = {
    val refs = reachable(root)
    for (r <- refs) {
      val dnfShrub = ShrubDNF.dnf(r.get).toShrub
      r.set(dnfShrub)
    }
  }

  def reduce(root: Ref): Unit = {
    convertToDNF(root)
    removeNonterminatingLoops(root)
  }

}
