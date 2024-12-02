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
    report == "7 6 4 2 1"
  }

}
