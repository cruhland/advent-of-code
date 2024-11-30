package net.cruhland.aoc.y2023

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day1 {
  private val NumbersPattern = "[1-9]"
  private val WordsPattern = "one|two|three|four|five|six|seven|eight|nine"

  val Numbers: Regex = NumbersPattern.r
  val WordsOrNumbersForward: Regex = s"$NumbersPattern|$WordsPattern".r
  val WordsOrNumbersReverse: Regex =
    s"$NumbersPattern|${WordsPattern.reverse}".r

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

  def solution1(input: String): Int = {
    common(input, firstNumberToDigit, lastNumberToDigit)
  }

  def firstNumberToDigit(line: String): Option[Int] = {
    Numbers.findFirstIn(line).flatMap(numberOrWordToDigit)
  }

  def lastNumberToDigit(line: String): Option[Int] = {
    Numbers.findFirstIn(line.reverse).flatMap(numberOrWordToDigit)
  }

  def solution2(input: String): Int = {
    common(input, firstNumberOrWordToDigit, lastNumberOrWordToDigit)
  }

  def firstNumberOrWordToDigit(line: String): Option[Int] = {
    WordsOrNumbersForward.findFirstIn(line).flatMap(numberOrWordToDigit)
  }

  def lastNumberOrWordToDigit(line: String): Option[Int] = {
    WordsOrNumbersReverse
      .findFirstIn(line.reverse)
      .map(_.reverse)
      .flatMap(numberOrWordToDigit)
  }

  def numberOrWordToDigit(numberOrWord: String): Option[Int] = {
    numberOrWord
      .headOption
      .collect { case c if c.isDigit => c - '0' }
      .orElse(DigitForWord.get(numberOrWord))
  }

  def common(
    input: String,
    findFirst: String => Option[Int],
    findLast: String => Option[Int],
  ): Int = {
    input
      .linesIterator
      .flatMap { line =>
        for {
          first <- findFirst(line)
          last <- findLast(line)
        } yield 10 * first + last
      }
      .sum
  }

}
