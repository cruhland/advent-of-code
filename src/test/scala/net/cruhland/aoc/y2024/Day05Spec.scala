package net.cruhland.aoc.y2024

import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day05Spec extends AnyFreeSpec with Matchers {

  "validate" - {
    def validate[A](rules: List[(A, A)], update: List[A]): Option[Boolean] = {
      when(hasUniqueElements(update)) {
        Day05.validate(rules)(update)
      }
    }

    "no rules, update always valid" - {
      def testNoRules[A](update: List[A]): Option[Boolean] = {
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
      ): Option[Boolean] = {
        val rules = (a1 -> a2) :: otherRules
        val update = prefix ++ (a2 :: middle) ++ (a1 :: suffix)
        validate(rules, update).map(!_)
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
      ): Option[Boolean] = {
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
        whenM(Day05.validate(rules)(update) == result && havePermutation) {
          validate(rulesPermuted, update).map(_ == result)
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

    "ignore rules whose left element is not present" - {
      def testMissingLeft[A](
        ruleLeft: A,
        ruleRight: A,
        prefix: List[A],
        suffix: List[A],
      ): Option[Boolean] = {
        whenM(!prefix.contains(ruleLeft) && !suffix.contains(ruleLeft)) {
          val rules = List(ruleLeft -> ruleRight)
          val update = prefix ++ (ruleRight :: suffix)
          validate(rules, update)
        }
      }

      "example 1" in {
        assert(testMissingLeft(
          ruleLeft = '?',
          ruleRight = 'z',
          prefix = List('x'),
          suffix = List('y'),
        ))
      }
    }

    "ignore rules whose right element is not present" - {
      def testMissingRight[A](
        ruleLeft: A,
        ruleRight: A,
        prefix: List[A],
        suffix: List[A],
      ): Option[Boolean] = {
        whenM(!prefix.contains(ruleRight) && !suffix.contains(ruleRight)) {
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

  "solution1" - {

    "example" in {
      val input =
        """47|53
          |97|13
          |97|61
          |97|47
          |75|29
          |61|13
          |75|53
          |29|13
          |97|29
          |53|29
          |61|53
          |97|53
          |61|29
          |47|13
          |75|47
          |97|75
          |47|61
          |75|61
          |47|29
          |75|13
          |53|13
          |
          |75,47,61,53,29
          |97,61,53,29,13
          |75,29,13
          |75,97,47,61,53
          |61,13,29
          |97,13,75,29,47
          |""".stripMargin

      val answer = Day05.solution1(input)
      answer mustBe 143
    }
  }

  def assert(condOpt: Option[Boolean]): Assertion = {
    assert(condOpt == Some(true))
  }

  def when(cond: Boolean)(result: => Boolean): Option[Boolean] = {
    Option.when(cond)(result)
  }

  def whenM(cond: Boolean)(result: => Option[Boolean]): Option[Boolean] = {
    Option.when(cond)(result).flatten
  }

  def hasUniqueElements[A](xs: List[A]): Boolean = xs.distinct.size == xs.size

}
