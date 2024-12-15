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
    val width = input.indexOf('\n')
    val height = input.count(_ == '\n') + 1
    StartingState(width = width, height = height)
  }

  case class StartingState(width: Int, height: Int)

}
