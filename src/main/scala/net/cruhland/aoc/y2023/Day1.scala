package net.cruhland.aoc.y2023

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day1 {

  def solution1(input: String): Int = {
    common(input)(line => line.collect { case c if c.isDigit => c - '0' })
  }

  def solution2(input: String): Int = {
    common(input)(wordsAndNumbersToDigits(_))
  }

  val Words: Regex = "one|two|three|four|five|six|seven|eight|nine".r
  val Numbers: Regex = "[0-9]".r

  val DigitForWord: Map[String, Int] = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )

  @tailrec
  def wordsAndNumbersToDigits(
    remainingLine: String,
    accum: List[Int] = Nil,
  ): List[Int] = {
    val (incr, digitOpt) =
      (Words
        .findPrefixOf(remainingLine)
        .map(prefix => (prefix.size, DigitForWord.get(prefix)))
      ).orElse(Numbers
        .findPrefixOf(remainingLine)
        .map(prefix => (prefix.size, prefix.headOption.map(_ - '0')))
      ).getOrElse((math.min(remainingLine.size, 1), None))

    if (incr > 0) {
      val newRemaining = remainingLine.substring(incr)
      val newAccum = digitOpt.map(_ :: accum).getOrElse(accum)
      wordsAndNumbersToDigits(newRemaining, newAccum)
    } else accum.reverse
  }

  def common(input: String)(lineToDigits: String => Iterable[Int]): Int = {
    input
      .linesIterator
      .map { line =>
        val digits = lineToDigits(line)
        if (digits.isEmpty) 0
        else {
          // These are safe because there is at least one digit
          val first = digits.head
          val last = digits.last

          10 * first + last
        }
      }
      .sum
  }

}
