package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day05Spec extends AnyFreeSpec with Matchers {
  import Day05._

  "OrderingRules.validate" - {

    "no rules, update always valid" in {
      val rules = new OrderingRules[Char]()
      val update = Seq('a', 'b', 'c')
      val result = rules.validate(update)
      result mustBe true
    }

    "one rule, update invalid" in {
      val rules = new OrderingRules('a' -> 'b')
      val update = Seq('b', 'a')
      val result = rules.validate(update)
      result mustBe false
    }

    "one rule, update valid" in {
      val rules = new OrderingRules('c' -> 'd')
      val update = Seq('c', 'd')
      val result = rules.validate(update)
      result mustBe true
    }

    "one rule, larger update invalid" in {
      val rules = new OrderingRules('c' -> 'd')
      val update = Seq('a', 'b', 'd', 'c', 'e')
      val result = rules.validate(update)
      result mustBe false
    }

  }

  "solution1" - {

  }

}
