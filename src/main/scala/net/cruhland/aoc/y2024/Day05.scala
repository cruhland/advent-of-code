package net.cruhland.aoc.y2024

object Day05 {

  def solution1(input: String): Int = {
    // Parse input into data structures (ordering rules, possible updates)
    // Keep only the updates that are valid according to the rules
    // Add up the middle page numbers of the valid updates
    ???
  }

  /** Check that a safety manual update obeys all rules.
    *
    * @param rules The rules to check against the update.
    * @param update The update to check. Must not contain duplicate elements.
    *
    * @return
    *   `true` if all elements of the update are in the order specified by the
    *   rules; `false` otherwise.
    */
  def validate[A](rules: Iterable[(A, A)], update: Seq[A]): Boolean = {
    val indices = update
      .iterator
      .zipWithIndex
      // Only correct when the original collection has no duplicates
      .toMap

    rules.forall { case (x, y) =>
      val xIndex = indices.getOrElse(x, -1)
      val yIndex = indices.getOrElse(y, update.size)
      xIndex < yIndex
    }
  }

}
