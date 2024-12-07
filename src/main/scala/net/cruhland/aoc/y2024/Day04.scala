package net.cruhland.aoc.y2024

object Day04 {

  def solution1(input: String): Int = {
    // Make a 2D grid of chars
    val charMatrix = input
      .split('\n')
      .toVector
      .map(Vector(_: _*))

    // Projections of the grid into rows by direction
    val horizontal = charMatrix.view
    val vertical = charMatrix.transpose(identity).view

    // Count words forward and backward in each projection
    List(horizontal, vertical)
      .flatMap(rows => List(rows, rows.map(_.reverse)))
      .map(countWords)
      .sum
  }

  private def countWords(lines: Iterable[Vector[Char]]): Int = {
    lines
      .map { line =>
        line
          .sliding(size = 4)
          .count(_ == WordVec)
      }
      .sum
  }

  private val WordVec: Vector[Char] = Vector("XMAS": _*)

}
