package net.cruhland.aoc.y2024

import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day06Spec extends AnyFreeSpec with Matchers {

  "parse" - {

    def testParse(
      rowCount: Int,
      colCount: Int,
      obstacles: Set[Day06.Location],
      guardOpt: Option[Day06.Guard],
    ): Errors = {
      val input = (0 until rowCount)
        .map { rowIndex =>
          (0 until colCount)
            .map { colIndex =>
              val loc = (rowIndex, colIndex)

              guardOpt match {
                case Some(guard) if guard.loc == loc =>
                  guard.dir match {
                    case Day06.North => '^'
                    case Day06.South => 'V'
                    case Day06.East => '>'
                    case Day06.West => '<'
                  }
                case _ =>
                  if (obstacles(loc)) '#' else '.'
              }
            }
            .mkString
        }
        .mkString("\n")

      val startState = Day06.parse(input)
      val obstaclesCorrect =
        haveSameElements(startState.obstacles, obstacles.toSeq)

      List(
        Option.unless(startState.rowCount == rowCount)("rowCount"),
        Option.unless(startState.colCount == colCount)("colCount"),
        Option.unless(obstaclesCorrect)("obstacles"),
        Option.unless(startState.guardOpt == guardOpt)("guardOpt"),
      ).flatten
    }

    "small blank map" in {
      assert(testParse(
        rowCount = 3,
        colCount = 4,
        obstacles = Set(),
        guardOpt = None,
      ))
    }

    "medium blank map" in {
      assert(testParse(
        rowCount = 71,
        colCount = 17,
        obstacles = Set(),
        guardOpt = None,
      ))
    }

    "tiny map with obstacles" in {
      assert(testParse(
        rowCount = 2,
        colCount = 2,
        obstacles = Set((0, 1), (1, 0)),
        guardOpt = None,
      ))
    }

    "small map with obstacles" in {
      assert(testParse(
        rowCount = 3,
        colCount = 5,
        obstacles = Set((0, 2), (1, 0), (1, 4), (2, 1), (2, 3)),
        guardOpt = None,
      ))
    }

    "guard position and direction" in {
      assert(testParse(
        rowCount = 3,
        colCount = 3,
        obstacles = Set(),
        guardOpt = Some(Day06.Guard(loc = (1, 1), dir = Day06.North)),
      ))
    }
  }

  type Errors = List[String]

  def assert(errors: Errors): Assertion = errors mustBe Nil

  def haveSameElements[A](xs: Seq[A], ys: Seq[A]): Boolean = {
    xs.diff(ys).isEmpty && ys.diff(xs).isEmpty
  }

}
