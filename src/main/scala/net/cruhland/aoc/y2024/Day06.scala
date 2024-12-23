package net.cruhland.aoc.y2024

import scala.jdk.StreamConverters.StreamHasToScala

object Day06 {

  def solution1(input: String): Int = {
    val lines = input.lines().toScala(Vector).map(_.toVector)
    val initialGrid = Grid(lines)

    // Find the guard's location and direction
    val initialGuardOpt = initialGrid
      .collectFirstWithLoc {
        case '^' => north
        case 'V' => south
        case '>' => east
        case '<' => west
      }
      .map { case (velocity, loc) => Guard(loc = loc, dir = velocity) }

    // Step the guard forward until they leave the area
    val Some((_, finalGrid)) = Iterator
      .iterate((initialGuardOpt, initialGrid)) {
        case (None, grid) =>
          (None, grid)
        case (Some(guard), grid) =>
          // Look one step ahead
          val nextLoc = guard.loc + guard.dir
          val nextCell = grid.lift(nextLoc)
          nextCell match {
            case None =>
              // Leave area
              (None, grid)
            case Some('#') =>
              // Turn guard if blocked by obstacle
              (Some(guard.copy(dir = rotateRight(guard.dir))), grid)
            case Some(_) =>
              // Take step
              val guardAfterStep = guard.copy(loc = nextLoc)
              val gridAfterStep = grid.updated(nextLoc, '+')
              (Some(guardAfterStep), gridAfterStep)
          }
      }
      .dropWhile { case (guardOpt, _) => guardOpt.isDefined }
      .nextOption()

    finalGrid.count(c => c != '.' && c != '#')
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

    def updated(loc: Location, value: A): Grid[A] = {
      val row = cells(loc.rowIndex)
      val rowUpdated = row.updated(loc.colIndex, value)
      val cellsUpdated = cells.updated(loc.rowIndex, rowUpdated)
      Grid(cellsUpdated)
    }

    def count(p: A => Boolean): Int = {
      cells
        .iterator
        .flatten
        .count(p)
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
