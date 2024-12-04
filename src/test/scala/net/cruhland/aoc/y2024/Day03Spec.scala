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

  def aPart2Solution(solutionFn: String => Int): Unit = {

    "has same behavior as solution1" - {
      behave like aPart1Solution(solutionFn)
    }

    "example" in {
      val input =
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)" +
          "+mul(32,64](mul(11,8)undo()?mul(8,5))"
      val answer = solutionFn(input)
      answer mustBe 48
    }

    "don't() disables muls" in {
      val input = "don't()mul(23,45)mul(67,89)"
      val answer = solutionFn(input)
      answer mustBe 0
    }

    "do() re-enables muls" in {
      val input = "don't()mul(98,76)do()mul(54,32)mul(11,99)"
      val answer = solutionFn(input)
      answer mustBe (54 * 32 + 11 * 99)
    }

  }

  "Day03.solution2" - {
    behave like aPart2Solution(solution2)
  }

  "Day03.solution2Monadic" - {
    behave like aPart2Solution(solution2Monadic)
  }

}
