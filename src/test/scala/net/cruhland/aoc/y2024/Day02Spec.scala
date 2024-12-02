package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day02Spec extends AnyFreeSpec with Matchers {
  import Day02._

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

      val answer = solution1(input)
      answer mustBe 2
    }

    "example report 1" in {
      val input = "7 6 4 2 1"
      val answer = solution1(input)
      answer mustBe 1
    }

    "example report 2" in {
      val input = "1 2 7 8 9"
      val answer = solution1(input)
      answer mustBe 0
    }

  }

  "Day02.validReport" - {

    "example 1" in {
      validReport("7 6 4 2 1") mustBe true
    }

    "example 2" in {
      validReport("1 2 7 8 9") mustBe false
    }

    "all increasing 1" in {
      validReport("1 2 3") mustBe true
    }

    "all decreasing 1" in {
      validReport("9 8 7 6") mustBe true
    }

  }

}
