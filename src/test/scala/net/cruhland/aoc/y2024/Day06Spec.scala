package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day06Spec extends AnyFreeSpec with Matchers {

  "parse" - {

    "map dimensions" - {
      def testMapDimensions(width: Int, height: Int): Boolean = {
        val row = "." * width
        val input = Iterator
          .fill(height)(row)
          .mkString("\n")

        val startState = Day06.parse(input)
        startState.width == width && startState.height == height
      }

      "small map" in assert(testMapDimensions(width = 4, height = 3))
      "medium map" in assert(testMapDimensions(width = 17, height = 71))
    }
  }

}
