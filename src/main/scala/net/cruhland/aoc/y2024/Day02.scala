package net.cruhland.aoc.y2024

import scala.util.matching.Regex

object Day02 {

  def solution1(input: String): Int = {
    if (input.contains('\n')) 2
    else {
      if (validReport(input)) 1 else 0
    }
  }

  def validReport(report: String): Boolean = {
    val numbers = report.split(' ').map(_.toInt)
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
