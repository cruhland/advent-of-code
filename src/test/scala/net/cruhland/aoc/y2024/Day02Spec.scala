package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day02Spec extends AnyFreeSpec with Matchers {

  "Day02.solution1" - {

    "example input" in {
      val input =
        """7 6 4 2 1
          |1 2 7 8 9
          |9 7 6 2 1
          |1 3 2 4 5
          |8 6 4 4 1
          |1 3 6 7 9
        """.stripMargin

      val answer = Day02.solution1(input)
      answer mustBe 2
    }

  }

}
