package net.cruhland.aoc.y2024

import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day06Spec extends AnyFreeSpec with Matchers {

  "parse" - {

    def testParse(
      width: Int,
      height: Int,
      obstacles: List[Day06.Position],
    ): Errors = {
      val row = "." * width
      val input = Iterator
        .fill(height)(row)
        .mkString("\n")

      val startState = Day06.parse(input)

      List(
        Option.unless(startState.width == width)("width"),
        Option.unless(startState.height == height)("height"),
        Option.unless(haveSameElements(startState.obstacles, obstacles)) {
          "obstacles"
        },
      ).flatten
    }

    "small blank map" in {
      assert(testParse(width = 4, height = 3, obstacles = Nil))
    }

    "medium blank map" in {
      assert(testParse(width = 17, height = 71, obstacles = Nil))
    }
  }

  type Errors = List[String]

  def assert(errors: Errors): Assertion = errors mustBe Nil

  def haveSameElements[A](xs: Seq[A], ys: Seq[A]): Boolean = {
    xs.diff(ys).isEmpty && ys.diff(xs).isEmpty
  }

}
