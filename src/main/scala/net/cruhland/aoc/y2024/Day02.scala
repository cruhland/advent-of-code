package net.cruhland.aoc.y2024

import scala.util.matching.Regex

object Day02 {

  def solution1(input: String): Int = {
    input
      .linesIterator
      .count(validReport)
  }

  def solution2(input: String): Int = {
    input
      .linesIterator
      .count(validTolerantReport)
  }

  def validTolerantReport(report: String): Boolean = {
    true
  }

  def validReport(report: String): Boolean = {
    val numbers = report.split(' ').flatMap(_.toIntOption)
    if (numbers.isEmpty) false
    else {
      val adjacentDiffs = numbers
        .lazyZip(numbers.tail)
        .map(_ - _)

      val allIncreasing = adjacentDiffs.forall(_ < 0)
      val allDecreasing = adjacentDiffs.forall(_ > 0)
      val diffsInRange = adjacentDiffs
        .map(math.abs)
        .forall(d => 1 <= d && d <= 3)

      (allIncreasing || allDecreasing) && diffsInRange
    }
  }

}
