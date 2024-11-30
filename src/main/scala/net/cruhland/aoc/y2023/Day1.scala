package net.cruhland.aoc.y2023

object Day1 {

  def solution1(input: String): Int = {
    common(input)(line => line.collect { case c if c.isDigit => c - '0' })
  }

  def common(input: String)(lineToDigits: String => Iterable[Int]): Int = {
    input
      .linesIterator
      .map { line =>
        val digits = lineToDigits(line)
        if (digits.isEmpty) 0
        else {
          // These are safe because there is at least one digit
          val first = digits.head
          val last = digits.last

          10 * first + last
        }
      }
      .sum
  }

}
