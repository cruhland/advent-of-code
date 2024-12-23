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
    val grid = Grid(lines)

    // Find the guard's location and direction
    val Some(guard) = grid
      .collectFirstWithLoc {
        case '^' => north
        case 'V' => south
        case '>' => east
        case '<' => west
      }
      .map { case (velocity, loc) => Guard(loc = loc, dir = velocity) }

    // Turn guard if blocked by obstacle
    val guardNextLoc = guard.loc + guard.dir
    val blockedByObstacle = grid
      .lift(guardNextLoc)
      .exists(_ == '#')

    val unblockedGuard = if (blockedByObstacle) {
      guard.copy(dir = rotateRight(guard.dir))
    } else guard

    // Measure distance of guard from edge of area
    unblockedGuard.dir match {
      case `north` => unblockedGuard.loc.rowIndex + 1
      case `south` => grid.rowCount - unblockedGuard.loc.rowIndex
      case `west` => unblockedGuard.loc.colIndex + 1
      case `east` => grid.colCount - unblockedGuard.loc.colIndex
    }
  }

  case class Vec2[A] private(val vector: Vector[A]) extends AnyVal {
    def rowIndex: A = vector(0)
    def colIndex: A = vector(1)

    def +(other: Vec2[A])(implicit ev: Numeric[A]): Vec2[A] = {
      val sums = vector
        .lazyZip(other.vector)
        .map(ev.plus)

      new Vec2(sums)
    }
  }

  object Vec2 {
    def apply[A](rowIndex: A, colIndex: A): Vec2[A] = {
      Vec2(Vector(rowIndex, colIndex))
    }
  }

  type Location = Vec2[Int]
  type Velocity = Vec2[Int]

  def rotateRight(v: Velocity): Velocity = Vec2(v.colIndex, -v.rowIndex)

  val north: Velocity = Vec2(-1, 0)
  val east: Velocity = rotateRight(north)
  val south: Velocity = rotateRight(east)
  val west: Velocity = rotateRight(south)

  case class Guard(loc: Location, dir: Velocity)

  case class Grid[A](cells: IndexedSeq[IndexedSeq[A]])
      extends PartialFunction[Location, A] {
    val rowCount = cells.size
    val colCount = cells.lift(0).fold(0)(_.size)

    def apply(loc: Location): A = cells(loc.rowIndex)(loc.colIndex)
    def isDefinedAt(loc: Location): Boolean = {
      cells.isDefinedAt(loc.rowIndex) &&
        cells.lift(0).exists(_.isDefinedAt(loc.colIndex))
    }

    def collectFirstWithLoc[B](
      pf: PartialFunction[A, B],
    ): Option[(B, Location)] = {
      cells
        .iterator
        .zipWithIndex
        .flatMap { case (line, rowIndex) =>
          line
            .iterator
            .zipWithIndex
            .collectFirst {
              case (c, colIndex) if pf.isDefinedAt(c) =>
                val b = pf(c)
                val loc = Vec2(rowIndex, colIndex)
                (b, loc)
            }
        }
        .nextOption()
    }
  }

}
