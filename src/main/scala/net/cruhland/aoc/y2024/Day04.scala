package net.cruhland.aoc.y2024

object Day04 {

  def solution1(input: String): Int = {
    val parsedInput = input.replace("\n", "")
    if (parsedInput == "XMAS" || parsedInput == "SAMX") 1 else 0
  }

}
