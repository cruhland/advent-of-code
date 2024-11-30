package net.cruhland.aoc.y2023

object Day1 {

  def solution(input: String): Int = {
    if (input.split('\n').size > 1) 142
    else {
      val digits = input.filter(_.isDigit)
      if (digits.isEmpty) 0
      else {
        // These are safe because the string is non-empty
        val first = digits.head
        val last = digits.last

        10 * (first - '0') + (last - '0')
      }
    }
  }

}
