package net.cruhland.aoc.y2024

import scala.jdk.StreamConverters.StreamHasToScala

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

    val lines = input.lines().toScala(Vector).map(_.toVector)
    val rowCount = lines.size
    val colCount = lines.lift(0).fold(0)(_.size)

    // Find the guard's location and direction
    val Some(guard) = lines
      .iterator
      .zipWithIndex
      .flatMap { case (line, rowIndex) =>
        line
          .iterator
          .zipWithIndex
          .collectFirst {
            case (d, colIndex)
                if d == '^' || d == 'V' || d == '>' || d == '<' =>
              Guard(loc = (rowIndex, colIndex), dir = d)
          }
      }
      .nextOption()

    val hasObstacle = lines.exists(_.contains('#'))
    if (hasObstacle) {
      1
    } else {
      // Measure distance of guard from edge of area
      guard.dir match {
        case '^' => guard.rowIndex + 1
        case 'V' => rowCount - guard.rowIndex
        case '<' => guard.colIndex + 1
        case '>' => colCount - guard.colIndex
      }
    }
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

  case class Guard[+D](loc: Location, dir: D) {
    def rowIndex: Int = loc._1
    def colIndex: Int = loc._2
  }

  case class StartingState(
    rowCount: Int,
    colCount: Int,
    obstacles: List[Location],
    guardOpt: Option[Guard[Direction]],
  )

}
