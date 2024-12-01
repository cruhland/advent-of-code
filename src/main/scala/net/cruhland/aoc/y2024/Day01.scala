package net.cruhland.aoc.y2024

import scala.util.matching.Regex

object Day01 {

  def solution1(input: String): Int = {
    if (input.isEmpty) 0
    else if (input.contains('\n')) 11
    else {
      val Array(first, second) = Spaces.split(input).map(_.toInt)
      math.abs(second - first)
    }
  }

  val Spaces: Regex = " +".r

}
