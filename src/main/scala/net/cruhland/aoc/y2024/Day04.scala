package net.cruhland.aoc.y2024

object Day04 {

  def solution1(input: String): Int = {
    val charMatrix = input
      .split('\n')
      .toVector
      .map(Vector(_: _*))

    val horizontal = charMatrix.view
    val vertical = charMatrix.transpose(identity).view
    countWords(horizontal) + countWords(vertical)
  }

  private def countWords(lines: Iterable[Vector[Char]]): Int = {
    lines
      .map { line =>
        line
          .sliding(size = 4)
          .count(word => word == WordVec || word == WordVec.reverse)
      }
      .sum
  }

  private val WordVec: Vector[Char] = Vector("XMAS": _*)

}
