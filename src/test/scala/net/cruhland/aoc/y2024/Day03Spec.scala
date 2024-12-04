package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day03Spec extends AnyFreeSpec with Matchers {
  import Day03._

  def aPart1Solution(solutionFn: String => Int): Unit = {

    "example input" in {
      val input =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)" +
        "+mul(32,64]then(mul(11,8)mul(8,5))"

      val answer = solutionFn(input)
      answer mustBe 161
    }

    "single mul" in {
      val input = "mul(2,4)"
      val answer = solutionFn(input)
      answer mustBe 8
    }

    "only garbage" in {
      val input = "@#$%^&"
      val answer = solutionFn(input)
      answer mustBe 0
    }

  }

  "Day03.solution1" - {
    behave like aPart1Solution(solution1)
  }

  "Day03.solution2" - {
    "has same behavior as solution1" - {
      behave like aPart1Solution(solution2)
    }

    "example" in {
      val input =
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)" +
          "+mul(32,64](mul(11,8)undo()?mul(8,5))"
      val answer = solution2(input)
      answer mustBe 48
    }

    "don't() disables muls" in {
      val input = "don't()mul(23,45)mul(67,89)"
      val answer = solution2(input)
      answer mustBe 0
    }

  }

}
