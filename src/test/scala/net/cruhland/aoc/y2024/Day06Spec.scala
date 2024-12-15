package net.cruhland.aoc.y2024

import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day06Spec extends AnyFreeSpec with Matchers {

  "parse" - {

    def testParse(
      rowCount: Int,
      colCount: Int,
      obstacles: Set[Day06.Position],
    ): Errors = {
      val input = (0 until rowCount)
        .map { rowIndex =>
          (0 until colCount)
            .map(colIndex => if (obstacles((rowIndex, colIndex))) '#' else '.')
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
      ).flatten
    }

    "small blank map" in {
      assert(testParse(rowCount = 3, colCount = 4, obstacles = Set()))
    }

    "medium blank map" in {
      assert(testParse(rowCount = 71, colCount = 17, obstacles = Set()))
    }

    "tiny map with obstacles" in {
      assert(testParse(
        rowCount = 2,
        colCount = 2,
        obstacles = Set((0, 1), (1, 0)),
      ))
    }

    "small map with obstacles" in {
      assert(testParse(
        rowCount = 3,
        colCount = 5,
        obstacles = Set((0, 2), (1, 0), (1, 4), (2, 1), (2, 3)),
      ))
    }
  }

  type Errors = List[String]

  def assert(errors: Errors): Assertion = errors mustBe Nil

  def haveSameElements[A](xs: Seq[A], ys: Seq[A]): Boolean = {
    xs.diff(ys).isEmpty && ys.diff(xs).isEmpty
  }

}
