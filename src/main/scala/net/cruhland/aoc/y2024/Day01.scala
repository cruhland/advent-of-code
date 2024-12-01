package net.cruhland.aoc.y2024

import scala.util.matching.Regex

object Day01 {

  def solution1(input: String): Int = {
    val (firstList, secondList) = parseLists(input)

    // Sort both lists, pair them back together, and compute distance
    firstList
      .sorted
      .zip(secondList.sorted)
      .map { case (first, second) => math.abs(first - second) }
      .sum
  }

  def solution2(input: String): Int = {
    val (leftList, rightList) = parseLists(input)

    // Count occurrences of entries in right list
    val occurrences = rightList.groupMapReduce(identity)(_ => 1)(_ + _)

    // Similarity score
    leftList
      .map(n => occurrences.getOrElse(n, 0) * n)
      .sum
  }

  def parseLists(input: String): (Seq[Int], Seq[Int]) = {
    input
      .linesIterator
      .flatMap { line =>
        Spaces.split(line) match {
          case Array(first, second, _*) => Some((first.toInt, second.toInt))
          case _ => None
        }
      }
      .toSeq
      .unzip
  }

  val Spaces: Regex = " +".r

}
