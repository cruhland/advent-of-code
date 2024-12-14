package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day05Spec extends AnyFreeSpec with Matchers {
  import Day05._

  "OrderingRules.validate" - {

    "no rules, update always valid" - {
      def testNoRules[A](update: Seq[A]): Boolean = {
        val rules = new OrderingRules[A]()
        rules.validate(update)
      }

      "example 1" in { testNoRules(update = Seq('a', 'b', 'c')) mustBe true }
      "example 2" in { testNoRules(update = Seq('n', 'm')) mustBe true }
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

    "two rules, update invalid" in {
      val rules = new OrderingRules('p' -> 'q', 'r' -> 's')
      val update = Seq('p', 'q', 's', 'r')
      val result = rules.validate(update)
      result mustBe false
    }

    "one rule, non-adjacent update valid" in {
      val rules = new OrderingRules('f' -> 'g')
      val update = Seq('f', 'e', 'h', 'g')
      val result = rules.validate(update)
      result mustBe true
    }

    "one rule, non-adjacent update invalid" in {
      val rules = new OrderingRules('j' -> 'k')
      val update = Seq('k', 'l', 'j')
      val result = rules.validate(update)
      result mustBe false
    }

  }

  "solution1" - {

  }

}
