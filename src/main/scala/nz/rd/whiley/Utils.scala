package nz.rd.whiley

import scala.annotation.tailrec

object Utils {

  /**
   * Iterate on mutable state until a fixpoint is reached.
   */
  @tailrec
  def fixpoint(readState: => Any)(body: => Any): Unit = {
    val before = readState
    body
    val after = readState
    if (before != after) { fixpoint(readState)(body) }
  }

}