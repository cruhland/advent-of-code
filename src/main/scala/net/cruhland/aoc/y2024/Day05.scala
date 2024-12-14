package net.cruhland.aoc.y2024

object Day05 {

  def solution1(input: String): Int = {
    // Parse input into data structures
    val Array(rulesInput, updatesInput, _*) = EmptyLineRegex.split(input)
    val rules = rulesInput
      .split('\n')
      .iterator
      .map { ruleInput =>
        val Array(left, right) = ruleInput.split('|')
        left.toInt -> right.toInt
      }
      .toList
    val updates = updatesInput
      .split('\n')
      .iterator
      .map { updateInput =>
        updateInput
          .split(',')
          .iterator
          .map(_.toInt)
          .toList
      }
      .toList

    // Add up the middle page numbers of the valid updates
    updates
      .filter(validate(rules))
      .map { update =>
        val middleIndex = update.size / 2
        update(middleIndex)
      }
      .sum
  }

  private val EmptyLineRegex = "\n\n".r

  /** Check that a safety manual update obeys all rules.
    *
    * @param rules The rules to check against the update.
    * @param update The update to check. Must not contain duplicate elements.
    *
    * @return
    *   `true` if all elements of the update are in the order specified by the
    *   rules; `false` otherwise.
    */
  def validate[A](rules: Iterable[(A, A)])(update: Seq[A]): Boolean = {
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
