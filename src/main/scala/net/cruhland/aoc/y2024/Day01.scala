package net.cruhland.aoc.y2024

import scala.util.matching.Regex

object Day01 {

  def solution1(input: String): Int = {
    // Parse both lists from the input
    val (firstList, secondList) = input
      .linesIterator
      .flatMap { line =>
        Spaces.split(line) match {
          case Array(first, second, _*) => Some((first.toInt, second.toInt))
          case _ => None
        }
      }
      .toSeq
      .unzip

    // Sort both lists, pair them back together, and compute distance
    firstList
      .sorted
      .zip(secondList.sorted)
      .map { case (first, second) => math.abs(first - second) }
      .sum
  }

  val Spaces: Regex = " +".r

}
