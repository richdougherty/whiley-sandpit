package nz.rd.whiley

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable

sealed trait Shrub
object Shrub {
  type Id = Int

  final case object Any extends Shrub
  final case object Void extends Shrub
  final case object Null extends Shrub
  final case object Int extends Shrub
  final case class Negation(child: Shrub) extends Shrub
  final case class Union(children: List[Shrub]) extends Shrub
  final case class Intersection(children: List[Shrub]) extends Shrub

  final case class Product(children: List[Id]) extends Shrub
}

import Shrub.Id

final class Shrubbery private[Shrubbery] (
    var root: Id,
    val shrubs: mutable.Map[Id, Shrub],
    var nextId: Id) {

  override def toString: String = s"Shrubbery($root, $shrubs)"

  def freshId(): Id = {
    val id = nextId
    nextId += 1
    id
  }

  def apply(id: Id): Shrub = shrubs(id)
  def update(id: Id, n: Shrub): Unit = {
    shrubs.update(id, n)
  }
  def +=(entry: (Id, Shrub)): Unit = {
    shrubs += entry
  }
  def +=(n: Shrub): Id = {
    val id = freshId()
    this += (id, n)
    id
  }

  def accepts(v: Value): Boolean = {
    def rootAccepts(sid: Id, v: Value): Boolean = {
      val s: Shrub = shrubs(sid)
      branchAccepts(s, v)
    }
    def branchAccepts(s: Shrub, v: Value): Boolean = (s, v) match {
      case (Shrub.Any, _) => true
      case (Shrub.Void, _) => false
      case (Shrub.Int, Value.Int(_)) => true
      case (Shrub.Null, Value.Null) => true
      case (Shrub.Intersection(ss), _) => ss.forall(branchAccepts(_, v))
      case (Shrub.Union(ss), _) => ss.exists(branchAccepts(_, v))
      case (Shrub.Negation(c), _) => !branchAccepts(c, v)
      case (Shrub.Product(sids), Value.Product(vs)) =>
        sids.length == vs.length && (sids zip vs).forall { case (sid, v) => rootAccepts(sid, v) }
      case _ => false
    }
    rootAccepts(root, v)
  }
}

object Shrubbery {
  def empty: Shrubbery = new Shrubbery(0, mutable.Map.empty, 0)

  def fromTree(tree: Tree): Shrubbery = {
    val sy = fromTreeRaw(tree)
    ShrubberyAlgorithms.mergeIdentical(sy)
    sy
  }

  def fromTreeRaw(tree: Tree): Shrubbery = {

    val g = empty

    val treeShrubs = mutable.Map.empty[Tree, Id]

    def convertRoot(tree: Tree, namedTrees: Map[String, Tree]): Id = {
      treeShrubs.get(tree) match {
        case Some(id) =>
          id
        case None =>
          val id = g.freshId()
          treeShrubs += (tree -> id)
          val shrub = convert(tree, Set.empty, namedTrees)
          g += (id -> shrub)
          id
      }
    }

    def convert(tree: Tree, visitedNames: immutable.Set[String], namedTrees: Map[String, Tree]): Shrub = {
      tree match {
        case Tree.Any => Shrub.Any
        case Tree.Void => Shrub.Void
        case Tree.Null => Shrub.Null
        case Tree.Int => Shrub.Int
        case Tree.Negation(child) =>
          Shrub.Negation(convert(child, visitedNames, namedTrees))
        case Tree.Union(children) =>
          Shrub.Union(children.map(convert(_, visitedNames, namedTrees)))
        case Tree.Intersection(children) =>
          Shrub.Intersection(children.map(convert(_, visitedNames, namedTrees)))
        case Tree.Product(children) =>
          Shrub.Product(children.map(convertRoot(_, namedTrees)))
        case rec@Tree.Recursive(name, body) =>
          convert(body, visitedNames + name, namedTrees + (name -> body))
        case Tree.Variable(name) if visitedNames.contains(name) => Shrub.Void
        case Tree.Variable(name) =>
          convert(namedTrees(name), visitedNames + name, namedTrees)
      }
    }
    g.root = convertRoot(tree, Map.empty)
    g
  }
}

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
}
