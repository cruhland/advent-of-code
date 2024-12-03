package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day03Spec extends AnyFreeSpec with Matchers {
  import Day03._

  "Day03.solution1" - {

    "example input" in {
      val input =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)" +
        "+mul(32,64]then(mul(11,8)mul(8,5))"

      val answer = solution1(input)
      answer mustBe 161
    }
  }
}
