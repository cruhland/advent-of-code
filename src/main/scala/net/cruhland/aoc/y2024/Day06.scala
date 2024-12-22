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
            case ('^', colIndex) => (north, colIndex)
            case ('V', colIndex) => (south, colIndex)
            case ('>', colIndex) => (east, colIndex)
            case ('<', colIndex) => (west, colIndex)
          }
          .map { case (velocity, colIndex) =>
            Guard(loc = Vec2(rowIndex, colIndex), dir = velocity)
          }
      }
      .nextOption()

    val guardNextRowIndex = guard.loc.rowIndex + guard.dir.rowIndex
    val guardNextColIndex = guard.loc.colIndex + guard.dir.colIndex
    val guardNextLoc = Vec2(guardNextRowIndex, guardNextColIndex)
    val blockedByObstacle = lines
      .lift(guardNextLoc.rowIndex)
      .flatMap(row => row.lift(guardNextLoc.colIndex))
      .exists(_ == '#')
    if (blockedByObstacle) {
      1
    } else {
      // Measure distance of guard from edge of area
      guard.dir match {
        case `north` => guard.loc.rowIndex + 1
        case `south` => rowCount - guard.loc.rowIndex
        case `west` => guard.loc.colIndex + 1
        case `east` => colCount - guard.loc.colIndex
      }
    }
  }

  case class Vec2[A] private(val vector: Vector[A]) extends AnyVal {
    def rowIndex: A = vector(0)
    def colIndex: A = vector(1)
  }

  object Vec2 {
    def apply[A](rowIndex: A, colIndex: A): Vec2[A] = {
      Vec2(Vector(rowIndex, colIndex))
    }
  }

  type Location = Vec2[Int]
  type Velocity = Vec2[Int]

  val north: Velocity = Vec2(-1, 0)
  val south: Velocity = Vec2(1, 0)
  val east: Velocity = Vec2(0, 1)
  val west: Velocity = Vec2(0, -1)

  case class Guard(loc: Location, dir: Velocity)

}
