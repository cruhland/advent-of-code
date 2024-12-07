package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day04Spec extends AnyFreeSpec with Matchers {
  import Day04._

  "Day04.solution1" - {

    "single horizontal word" in {
      val input = "XMAS"
      val answer = solution1(input)
      answer mustBe 1
    }

    "horizontal non-word" in {
      val input = "MAXS"
      val answer = solution1(input)
      answer mustBe 0
    }

    "single backwards horizontal word" in {
      val input = "SAMX"
      val answer = solution1(input)
      answer mustBe 1
    }

    "single vertical word" in {
      val input =
        """X
          |M
          |A
          |S
          |""".stripMargin
      val answer = solution1(input)
      answer mustBe 1
    }

    "vertical non-word" in {
      val input =
        """X
          |M
          |A
          |X
          |""".stripMargin
      val answer = solution1(input)
      answer mustBe 0
    }

    "single backwards vertical word" in {
      val input =
        """S
          |A
          |M
          |X
          |""".stripMargin
      val answer = solution1(input)
      answer mustBe 1
    }

  }

}
