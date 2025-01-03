package net.cruhland.aoc.y2024

import scala.util.matching.Regex

object Day02 {

  def common(input: String, isReportValid: Vector[Int] => Boolean): Int = {
    input
      .linesIterator
      .map(parseReport)
      .count(isReportValid)
  }

  def solution1(input: String): Int = common(input, validNumbers)

  def solution2(input: String): Int = common(input, validTolerantNumbers)

  def validTolerantReport(report: String): Boolean = {
    val numbers = parseReport(report)
    validTolerantNumbers(numbers)
  }

  def validTolerantNumbers(numbers: Vector[Int]): Boolean = {
    if (numbers.isEmpty) false
    else if (validNumbers(numbers)) true
    else {
      val inits = numbers.inits.toList.tail.reverse
      val tails = numbers.tails.toList.tail

      inits
        .lazyZip(tails)
        .map(_ ++ _)
        .exists(validNumbers)
    }
  }

  def validReport(report: String): Boolean = {
    val numbers = parseReport(report)
    validNumbers(numbers)
  }

  def validNumbers(numbers: Vector[Int]): Boolean = {
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

  def parseReport(report: String): Vector[Int] = {
    report
      .split(' ')
      .flatMap(_.toIntOption)
      .toVector
  }

}
