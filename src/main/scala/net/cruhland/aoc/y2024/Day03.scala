package net.cruhland.aoc.y2024

import scala.util.matching.Regex

object Day03 {

  def solution1(input: String): Int = {
    MulRegex
      .findAllMatchIn(input)
      .map { matchData =>
        matchData
          .subgroups
          .map(_.toInt)
          .product
      }
      .sum
  }

  val MulRegex: Regex = raw"mul\((\d+),(\d+)\)".r

}
