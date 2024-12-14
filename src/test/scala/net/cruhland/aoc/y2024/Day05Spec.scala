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

      "example 1" in assert(testNoRules(update = Seq('a', 'b', 'c')))
      "example 2" in assert(testNoRules(update = Seq('n', 'm')))
    }

    "update invalid when elements are in the reverse order of a rule" - {
      def testSwap[A](
        a1: A,
        a2: A,
        prefix: List[A],
        middle: List[A],
        suffix: List[A],
        otherRules: List[(A, A)],
      ): Boolean = {
        val rules = new OrderingRules((a1 -> a2) :: otherRules: _*)
        val update = prefix ++ (a2 :: middle) ++ (a1 :: suffix)
        !rules.validate(update)
      }

      "example 1" in {
        val prefix = List()
        val middle = List()
        val suffix = List()
        val otherRules = List()
        assert(testSwap('a', 'b', prefix, middle, suffix, otherRules))
      }

      "example 2" in {
        val prefix = List()
        val middle = List()
        val suffix = List()
        val otherRules = List()
        assert(testSwap('x', 'y', prefix, middle, suffix, otherRules))
      }

      "example 3" in {
        val prefix = List('r')
        val middle = List('s')
        val suffix = List('t')
        val otherRules = List()
        assert(testSwap('p', 'q', prefix, middle, suffix, otherRules))
      }

      "example 4" in {
        val prefix = List('p', 'q')
        val middle = List()
        val suffix = List()
        val otherRules = List('p' -> 'q')
        assert(testSwap('r', 's', prefix, middle, suffix, otherRules))
      }
    }

    "one rule, update valid" - {
      def testSame[A](
        a1: A,
        a2: A,
        prefix: List[A],
        middle: List[A],
        suffix: List[A],
      ): Boolean = {
        val rules = new OrderingRules(a1 -> a2)
        val update = prefix ++ (a1 :: middle) ++ (a2 :: suffix)
        rules.validate(update)
      }

      "example 1" in {
        val prefix = List()
        val middle = List()
        val suffix = List()
        assert(testSame('c', 'd', prefix, middle, suffix))
      }

      "example 2" in {
        val prefix = List('f')
        val middle = List('g')
        val suffix = List('h')
        assert(testSame('i', 'j', prefix, middle, suffix))
      }

      "example 3" in {
        val prefix = List()
        val middle = List('e', 'h')
        val suffix = List()
        assert(testSame('f', 'g', prefix, middle, suffix))
      }
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
