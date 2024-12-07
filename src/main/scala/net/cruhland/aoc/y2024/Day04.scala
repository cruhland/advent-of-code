package net.cruhland.aoc.y2024

object Day04 {

  def solution1(input: String): Int = {
    // Make a 2D grid of chars
    val charMatrix = input
      .split('\n')
      .toVector
      .map(Vector(_: _*))
    val rowCount = charMatrix.size
    val colCount = charMatrix.lift(0).fold(0)(_.size)

    // Projections of the grid into rows by direction
    val horizontal = charMatrix
    val vertical = charMatrix.transpose(identity)
    val diagonalUp = for {
      n <- (0 to (rowCount - 1 + colCount - 1))
    } yield {
      for {
        c <- (0 to n)
        r = n - c
        if r < rowCount && c < colCount
      } yield charMatrix(r)(c)
    }
    val diagonalDown = for {
      n <- (0 to (rowCount - 1 + colCount - 1))
    } yield {
      for {
        c <- (0 to n)
        r = rowCount - 1 - (n - c)
        if 0 <= r && r < rowCount && 0 <= c && c < colCount
      } yield charMatrix(r)(c)
    }

    // Count words forward and backward in each projection
    val possibleWords = for {
      projection <- Iterator(horizontal, vertical, diagonalUp, diagonalDown)
      row <- projection.view
      possibleWord <- row.sliding(size = WordVec.size)
    } yield possibleWord

    possibleWords.count(word => word == WordVec || word.reverse == WordVec)
  }

  private val WordVec: Vector[Char] = Vector("XMAS": _*)

}
