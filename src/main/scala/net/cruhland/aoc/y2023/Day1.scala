package net.cruhland.aoc.y2023

object Day1 {

  def solution(input: String): Int = {
    input
      .linesIterator
      .map { line =>
        val digits = line.filter(_.isDigit)
        if (digits.isEmpty) 0
        else {
          // These are safe because the string is non-empty
          val first = digits.head
          val last = digits.last

          10 * (first - '0') + (last - '0')
        }
      }
      .sum
  }

}
