package net.cruhland.aoc.y2024

object Day05 {

  def solution1(input: String): Int = {
    // Parse input into data structures (ordering rules, possible updates)
    // Keep only the updates that are valid according to the rules
    // Add up the middle page numbers of the valid updates
    ???
  }

  def validate[A](rules: Iterable[(A, A)], update: Seq[A]): Boolean = {
    rules.forall { case (x, y) =>
      val yIndex = update.indexOf(y)
      yIndex == -1 || update.indexOf(x) < yIndex
    }
  }

}
