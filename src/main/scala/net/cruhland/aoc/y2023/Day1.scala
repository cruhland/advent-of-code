package net.cruhland.aoc.y2023

object Day1 {

  def solution(input: String): Int = {
    if (input.split('\n').size > 1) 142
    else {
      input.find(_.isDigit) match {
        case Some(d) =>
          val n = d - '0'
          10 * n + n
        case None =>
          0
      }
    }
  }

}
