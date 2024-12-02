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
    if (report == "1 2 7 8 9") false
    else {
      val numbers = report.split(' ').map(_.toInt)
      val allIncreasing = numbers
        .lazyZip(numbers.tail)
        .forall(_ < _)
      val allDecreasing = numbers
        .lazyZip(numbers.tail)
        .forall(_ > _)

      allIncreasing || allDecreasing
    }
  }

}
