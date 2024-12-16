package net.cruhland.aoc.y2024

object Day06 {

  def solution1(input: String): Int = {
    // Parse the input into a model of the state
    // While the guard has not left the board, step the state forward
    // Count positions visited

    // The state model will need to have the following information:
    // - Static: Dimensions of the map
    // - Static: Positions of all obstacles
    // - Dynamic: Optional position of the guard (None if not on map)
    // - Dynamic: Direction of the guard
    // - Dynamic: Which positions have been visited (to provide count)
    ???
  }

  def parse(input: String): StartingState = {
    val rows = input.split('\n')

    val obstacles = rows
      .iterator
      .zipWithIndex
      .flatMap { case (row, rowIndex) =>
        row
          .iterator
          .zipWithIndex
          .collect { case ('#', colIndex) => (rowIndex, colIndex) }
      }
      .toList

    val guardOpt = rows
      .iterator
      .zipWithIndex
      .flatMap { case (row, rowIndex) =>
        row
          .iterator
          .zipWithIndex
          .collect { case ('^', colIndex) =>
            Guard(loc = (rowIndex, colIndex), dir = North)
          }
      }
      .nextOption()

    StartingState(
      rowCount = rows.size,
      colCount = rows(0).size,
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
