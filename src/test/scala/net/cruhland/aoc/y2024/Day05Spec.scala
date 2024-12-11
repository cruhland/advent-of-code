package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day05Spec extends AnyFreeSpec with Matchers {
  import Day05._

  "Day05.isUpdateValid" - {

    "no rules, update always valid" in {
      val rules = new OrderingRules
      val update = Seq('a', 'b', 'c')
      val result = isUpdateValid(update, rules)
      result mustBe true
    }

  }

  "Day05.solution1" - {

  }

}
