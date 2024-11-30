package net.cruhland.aoc.y2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day1Spec extends AnyFreeSpec with Matchers {

  "Day1.solution" - {
    "works on the provided example" in {
      val input =
        """1abc2
          |pqr3stu8vwx
          |a1b2c3d4e5f
          |treb7uchet
        """.stripMargin

      val answer = Day1.solution(input)
      answer mustBe 142
    }

    "empty input" in {
      val input = ""
      val answer = Day1.solution(input)
      answer mustBe 0
    }

    "single line, no digits" in {
      val input = "abc"
      val answer = Day1.solution(input)
      answer mustBe 0
    }
  }

}
