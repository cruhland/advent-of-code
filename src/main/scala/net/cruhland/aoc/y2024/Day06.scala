package net.cruhland.aoc.y2024

object Day06 {

  def solution1(input: String): Int = {
    // Parse the input into a model of the state
    // While the guard has not left the board, step the state forward
    // Count positions visited

    // The state model can mostly rely on the grid from the input:
    // - Blank positions
    // - Obstacle positions
    // - Visited positions
    // But there also needs to be state to track the guard:
    // - Current location
    // - Current direction

    input.count(_ == '.') + 1
  }

  def parse(input: String): StartingState = {
    val rows = input.split('\n')

    val (obstaclesWithLocs, guardsWithLocs) = rows
      .iterator
      .zipWithIndex
      .flatMap { case (row, rowIndex) =>
        row
          .iterator
          .zipWithIndex
          .collect {
            case (c, colIndex) if c != '.' => (c, (rowIndex, colIndex))
          }
      }
      .partition { case (c, _) => c == '#' }

    val obstacles = obstaclesWithLocs
      .map { case (_, loc) => loc }
      .toList

    val guardOpt = guardsWithLocs
      .map { case (c, loc) =>
        val dir = c match {
          case '^' => North
          case 'V' => South
          case '>' => East
          case '<' => West
        }

        Guard(loc, dir)
      }
      .nextOption()

    StartingState(
      rowCount = rows.size,
      colCount = rows.lift(0).fold(0)(_.size),
      obstacles = obstacles,
      guardOpt = guardOpt,
    )
  }

  /** Row index, column index */
  type Location = (Int, Int)

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction

  case class Guard(loc: Location, dir: Direction)

  case class StartingState(
    rowCount: Int,
    colCount: Int,
    obstacles: List[Location],
    guardOpt: Option[Guard],
  )

}
