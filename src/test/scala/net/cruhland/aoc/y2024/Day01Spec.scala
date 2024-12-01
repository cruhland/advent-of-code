package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day01Spec extends AnyFreeSpec with Matchers {

  "Day01.solution1" - {

    "is correct for the example input" in {
      val input =
        """3   4
          |4   3
          |2   5
          |1   3
          |3   9
          |3   3
        """.stripMargin

      val answer = Day01.solution1(input)
      answer mustBe 11
    }

    "empty input" in {
      val input = ""
      val answer = Day01.solution1(input)
      answer mustBe 0
    }

  }

}
