package net.cruhland.aoc.y2024

import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day06Spec extends AnyFreeSpec with Matchers {

  "parse" - {

    def testParse(
      width: Int,
      height: Int,
      obstacles: Set[Day06.Position],
    ): Errors = {
      val input = (0 until height)
        .map { y =>
          (0 until width)
            .map(x => if (obstacles((x, y))) '#' else '.')
            .mkString
        }
        .mkString("\n")

      val startState = Day06.parse(input)
      val obstaclesCorrect =
        haveSameElements(startState.obstacles, obstacles.toSeq)

      List(
        Option.unless(startState.width == width)("width"),
        Option.unless(startState.height == height)("height"),
        Option.unless(obstaclesCorrect)("obstacles"),
      ).flatten
    }

    "small blank map" in {
      assert(testParse(width = 4, height = 3, obstacles = Set()))
    }

    "medium blank map" in {
      assert(testParse(width = 17, height = 71, obstacles = Set()))
    }

    "tiny map with obstacles" in {
      assert(testParse(
        width = 2,
        height = 2,
        obstacles = Set((0, 1), (1, 0)),
      ))
    }

    "small map with obstacles" in {
      assert(testParse(
        width = 5,
        height = 3,
        obstacles = Set((2, 0), (0, 1), (4, 1), (1, 2), (3, 2)),
      ))
    }
  }

  type Errors = List[String]

  def assert(errors: Errors): Assertion = errors mustBe Nil

  def haveSameElements[A](xs: Seq[A], ys: Seq[A]): Boolean = {
    xs.diff(ys).isEmpty && ys.diff(xs).isEmpty
  }

}
