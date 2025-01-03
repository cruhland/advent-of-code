package net.cruhland.aoc.y2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day1Spec extends AnyFreeSpec with Matchers {

  def part1Solution(solutionFn: String => Int): Unit = {
    "works on the provided example" in {
      val input =
        """1abc2
          |pqr3stu8vwx
          |a1b2c3d4e5f
          |treb7uchet
        """.stripMargin

      val answer = solutionFn(input)
      answer mustBe 142
    }

    "empty input" in {
      val input = ""
      val answer = solutionFn(input)
      answer mustBe 0
    }

    "single line, no digits" in {
      val input = "abc"
      val answer = solutionFn(input)
      answer mustBe 0
    }

    "single line, one digit" in {
      val input = "a5c"
      val answer = solutionFn(input)
      answer mustBe 55
    }

    "single line, two digits" in {
      val input = "3b8"
      val answer = solutionFn(input)
      answer mustBe 38
    }

    "two lines, mixed digits" in {
      val input = "a1b2c3d\nx8yz"
      val answer = solutionFn(input)
      answer mustBe (13 + 88)
    }
  }

  "Day1.solution1" - {
    behave like part1Solution(Day1.solution1)
  }

  "Day1.solution2" - {
    behave like part1Solution(Day1.solution2)

    "works on the part 2 example" in {
      val input =
        """two1nine
          |eightwothree
          |abcone2threexyz
          |xtwone3four
          |4nineeightseven2
          |zoneight234
          |7pqrstsixteen
        """.stripMargin

      val answer = Day1.solution2(input)
      answer mustBe 281
    }

    "handles edge case" in {
      val input = "oneight"
      val answer = Day1.solution2(input)
      answer mustBe 18
    }
  }

}
