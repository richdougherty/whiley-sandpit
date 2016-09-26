package nz.rd.whiley

import scala.collection.mutable
import Shrub.Id

import scala.annotation.tailrec

object ShrubberyAlgorithms {

  def mergeIdentical(sy: Shrubbery): Unit = {
    val replacements = mutable.Map.empty[Id, Id]
    val index = mutable.Map.empty[Shrub, Id]
    for ((sid, shrub) <- sy.shrubs) {
      index.get(shrub) match {
        case Some(existingSid) =>
          // We found an identical shrub, record the ids for later updating
          replacements += (sid -> existingSid)
        case None =>
          // New shrub for the index
          index += (shrub -> sid)
      }
    }
    // Remove duplicate shrubs
    for (sid <- replacements.keys) {
      sy.shrubs - sid
    }
    // Convert existing shrubs
    def replacedId(id: Id): Id = replacements.getOrElse(id, id)
    sy.root = replacedId(sy.root)
    for ((sid, existingShrub) <- sy.shrubs.toList) {
      def convert(s: Shrub): Shrub = s match {
        case Shrub.Any => Shrub.Any
        case Shrub.Void => Shrub.Void
        case Shrub.Null => Shrub.Null
        case Shrub.Int => Shrub.Int
        case Shrub.Negation(child) =>
          Shrub.Negation(convert(child))
        case Shrub.Union(children) =>
          Shrub.Union(children.map(convert(_)))
        case Shrub.Intersection(children) =>
          Shrub.Intersection(children.map(convert(_)))
        case p@Shrub.Product(children) =>
          val p2 = Shrub.Product(children.map(replacedId(_)))
          p2
      }
      sy.shrubs(sid) = convert(existingShrub)
    }
  }

  def garbageCollect(sy: Shrubbery): Unit = {
    def shrubRefs(s: Shrub): Set[Id] = s match {
      case Shrub.Any | Shrub.Void | Shrub.Null | Shrub.Int => Set.empty
      case Shrub.Negation(child) => shrubRefs(child)
      case Shrub.Union(children) => children.flatMap(shrubRefs(_)).toSet
      case Shrub.Intersection(children) => children.flatMap(shrubRefs(_)).toSet
      case p@Shrub.Product(children) => children.toSet
    }
    @tailrec
    def allRefs(toVisit: List[Id], seen: Set[Id]): Set[Id] = toVisit match {
      case Nil =>
        seen
      case head::tail =>
        val newRefs = shrubRefs(sy(head))
        allRefs(tail ++ (newRefs diff seen), seen union newRefs)
    }

    val reachable = allRefs(sy.root::Nil, Set(sy.root))
    val toRemove = sy.shrubs.keys.toSet diff reachable
    for (id <- toRemove) { sy.shrubs.remove(id) }
  }

  def simplifyIdentities(sy: Shrubbery): Unit = {
    val updated = sy.shrubs.mapValues(simplifyIdentities(_))
    sy.shrubs ++= updated
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
    case p@Shrub.Product(children) => p
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
  def removeNonterminatingLoops(sy: Shrubbery): Unit = {
    val shrubTermination = mutable.Map[Id, Boolean]()

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

    Utils.fixpoint(shrubTermination.toMap) {
      for ((sid, shrub) <- sy.shrubs) {
        shrubTermination(sid) = mayTerminate(shrub)
      }
    }

    for (sid <- sy.shrubs.keys) {
      if (!shrubTermination(sid)) {
        sy.shrubs(sid) = Shrub.Void
      }
    }
  }
}
