package net.cruhland.aoc.y2024

object Day04 {

  def solution1(input: String): Int = {
    input
      .replace("\n", "")
      .sliding(size = 4)
      .count(word => word == "XMAS" || word == "SAMX")
  }

}
