package net.cruhland.aoc.y2024

object Day05 {

  def common(input: String)(
    collectFn:
        PartialFunction[(List[(Int, Int)], IndexedSeq[Int]), IndexedSeq[Int]],
  ): Int = {
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
          .toVector
      }
      .toList

    // Add up the middle page numbers of the selected updates
    updates
      .iterator
      .map(update => (rules, update))
      .collect(collectFn)
      .map { update =>
        val middleIndex = update.size / 2
        update(middleIndex)
      }
      .sum
  }

  def solution1(input: String): Int = {
    common(input) { case (rules, update) if validate(rules)(update) => update }
  }

  def solution2(input: String): Int = {
    common(input) { case (rules, update) if !validate(rules)(update) =>
      correctOrder(rules)(update)
    }
  }

  /** Rearrange the pages of a safety manual update into the correct order as
    * determined by the rules.
    *
    * @param rules
    *   The ordering rules. Each rule says that its first element must appear
    *   before its second element in any update.
    * @param update
    *   The safety manual update to rearrange.
    *
    * @return A permutation of the input update that's correct by the rules.
    */
  def correctOrder[A](rules: Iterable[(A, A)])(
    originalUpdate: IndexedSeq[A],
  ): IndexedSeq[A] = {
    Iterator
      .iterate[Option[IndexedSeq[A]]](Some(originalUpdate)) { prevUpdateOpt =>
        for {
          prevUpdate <- prevUpdateOpt
          (beforeIndex, afterIndex) <- findBrokenRule(rules)(prevUpdate)
        } yield {
          // Swap elements
          val before = prevUpdate(beforeIndex)
          val after = prevUpdate(afterIndex)
          val prevUpdate1 = prevUpdate.updated(beforeIndex, after)
          prevUpdate1.updated(afterIndex, before)
        }
      }
      .takeWhile(_.nonEmpty)
      .flatten
      // Take the last element; safe because there's always at least one
      .reduceLeft((_, next) => next)
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
    findBrokenRule(rules)(update).isEmpty
  }

  private def findBrokenRule[A](rules: Iterable[(A, A)])(
    update: Seq[A],
  ): Option[(Int, Int)] = {
    val indices = update
      .iterator
      .zipWithIndex
      // Only correct when the original collection has no duplicates
      .toMap

    rules
      .iterator
      .map { case (before, after) =>
        val beforeIndex = indices.getOrElse(before, -1)
        val afterIndex = indices.getOrElse(after, update.size)
        beforeIndex -> afterIndex
      }
      .find { case (beforeIndex, afterIndex) => beforeIndex >= afterIndex }
  }

}
