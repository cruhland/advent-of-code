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

  }

}
