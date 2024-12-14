package net.cruhland.aoc.y2024

import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day05Spec extends AnyFreeSpec with Matchers {
  import Day05._

  def assert(condOpt: Option[Boolean]): Assertion = {
    assert(condOpt == Some(true))
  }

  "OrderingRules.validate" - {

    "no rules, update always valid" - {
      def testNoRules[A](update: List[A]): Boolean = {
        validate(rules = List(), update)
      }

      "example 1" in assert(testNoRules(update = List('a', 'b', 'c')))
      "example 2" in assert(testNoRules(update = List('n', 'm')))
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
        val rules = (a1 -> a2) :: otherRules
        val update = prefix ++ (a2 :: middle) ++ (a1 :: suffix)
        !validate(rules, update)
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

      "example 5" in {
        val prefix = List()
        val middle = List('z', 'w')
        val suffix = List()
        val otherRules = List('z' -> 'w')
        assert(testSwap('x', 'y', prefix, middle, suffix, otherRules))
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
        val rules = List(a1 -> a2)
        val update = prefix ++ (a1 :: middle) ++ (a2 :: suffix)
        validate(rules, update)
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

    "the order of the rules doesn't matter" - {
      def haveSameElements[A](xs: List[A], ys: List[A]): Boolean = {
        xs.diff(ys).isEmpty && ys.diff(xs).isEmpty
      }

      def testRulesPermutation[A](
        rules: List[(A, A)],
        update: List[A],
        result: Boolean,
        rulesPermuted: List[(A, A)],
      ): Option[Boolean] = {
        val havePermutation = haveSameElements(rules, rulesPermuted)
        Option.when(validate(rules, update) == result && havePermutation) {
          validate(rulesPermuted, update) == result
        }
      }

      "example 1" in {
        val rules = List('a' -> 'b', 'c' -> 'd')
        val update = List('b', 'c', 'd', 'a')
        val result = false
        val rulesPermuted = List('c' -> 'd', 'a' -> 'b')
        assert(testRulesPermutation(rules, update, result, rulesPermuted))
      }
    }

    "ignore rules whose right element is not present" - {
      def testMissingRight[A](
        ruleLeft: A,
        ruleRight: A,
        prefix: List[A],
        suffix: List[A],
      ): Option[Boolean] = {
        val reqs = !prefix.contains(ruleRight) && !suffix.contains(ruleRight)
        Option.when(reqs) {
          val rules = List(ruleLeft -> ruleRight)
          val update = prefix ++ (ruleLeft :: suffix)
          validate(rules, update)
        }
      }

      "example 1" in {
        assert(testMissingRight(
          ruleLeft = 'a',
          ruleRight = '?',
          prefix = List('c'),
          suffix = List('b'),
        ))
      }
    }

  }

}
