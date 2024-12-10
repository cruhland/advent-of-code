package net.cruhland.aoc.y2024

object Day04 {

  def solution1(input: String): Int = {
    val gps = new GridProjections(input)

    // Count all words found in each projection
    Iterator(gps.horizontal, gps.vertical, gps.diagonalUp, gps.diagonalDown)
      .map(findAll(XmasVec))
      .map(_.size)
      .sum
  }

  private val XmasVec = Vector("XMAS": _*)

  private def findAll(word: Vector[Char])(
    projection: IndexedSeq[IndexedSeq[Cell]],
  ): IndexedSeq[IndexedSeq[Cell]] = {
    for {
      row <- projection
      possibleWord <- row.sliding(size = word.size)
      possibleLetters = possibleWord.map(_.letter)
      if possibleLetters == word || possibleLetters.reverse == word
    } yield possibleWord
  }

  def solution2(input: String): Int = {
    val gps = new GridProjections(input)

    // Count all X-MAS patterns by finding "MAS" that overlap at 'A'
    val aLocsUp = findAllALocsInMas(gps.diagonalUp)
    val aLocsDown = findAllALocsInMas(gps.diagonalDown)
    aLocsUp
      .intersect(aLocsDown)
      .size
  }

  private val MasVec = Vector("MAS": _*)
  private val IndexA = MasVec.indexWhere(_ == 'A')

  private def findAllALocsInMas(
    projection: IndexedSeq[IndexedSeq[Cell]],
  ): IndexedSeq[(Int, Int)] = {
    findAll(MasVec)(projection)
      .map(_(IndexA).loc)
  }

  private class GridProjections(input: String) {
    // Make a 2D grid of chars and their coordinates
    val horizontal: IndexedSeq[IndexedSeq[Cell]] = {
      val lines = input
        .split('\n')
        .toVector

      for {
        (line, rowIndex) <- lines.zipWithIndex
      } yield {
        for {
          (char, colIndex) <- line.zipWithIndex
        } yield Cell(char, (rowIndex, colIndex))
      }
    }

    private val rowCount = horizontal.size
    private val colCount = horizontal.lift(0).fold(0)(_.size)

    // Projections of the grid into rows by direction
    def vertical: IndexedSeq[IndexedSeq[Cell]] = horizontal.transpose(identity)

    def diagonalUp: IndexedSeq[IndexedSeq[Cell]] = {
      for {
        n <- (0 to (rowCount - 1 + colCount - 1))
      } yield {
        for {
          c <- (0 to n)
          r = n - c
          if r < rowCount && c < colCount
        } yield horizontal(r)(c)
      }
    }

    def diagonalDown: IndexedSeq[IndexedSeq[Cell]] = {
      for {
        n <- (0 to (rowCount - 1 + colCount - 1))
      } yield {
        for {
          c <- (0 to n)
          r = rowCount - 1 - (n - c)
          if 0 <= r && r < rowCount && 0 <= c && c < colCount
        } yield horizontal(r)(c)
      }
    }

  }

  private case class Cell(letter: Char, loc: (Int, Int))

}
