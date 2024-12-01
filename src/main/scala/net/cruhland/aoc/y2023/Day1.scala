package net.cruhland.aoc.y2023

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day1 {

  def common(
    input: String,
    findFirst: String => Option[String],
    findLast: String => Option[String],
  ): Int = {
    input
      .linesIterator
      .flatMap { line =>
        for {
          first <- findFirst(line)
          last <- findLast(line)
          firstDigit <- numberOrWordToDigit(first)
          lastDigit <- numberOrWordToDigit(last)
        } yield 10 * firstDigit + lastDigit
      }
      .sum
  }

  def solution1(input: String): Int = {
    common(input, firstNumber, lastNumber)
  }

  def firstNumber(line: String): Option[String] = {
    Numbers.findFirstIn(line)
  }

  def lastNumber(line: String): Option[String] = {
    Numbers.findFirstIn(line.reverse)
  }

  def solution2(input: String): Int = {
    common(input, firstNumberOrWord, lastNumberOrWord)
  }

  def firstNumberOrWord(line: String): Option[String] = {
    WordsOrNumbersForward.findFirstIn(line)
  }

  def lastNumberOrWord(line: String): Option[String] = {
    WordsOrNumbersReverse
      .findFirstIn(line.reverse)
      .map(_.reverse)
  }

  def numberOrWordToDigit(numberOrWord: String): Option[Int] = {
    numberOrWord
      .headOption
      .collect { case c if c.isDigit => c - '0' }
      .orElse(DigitForWord.get(numberOrWord))
  }

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

  val NumbersPattern: String = "[1-9]"
  val WordsPattern: String = DigitForWord.keys.mkString("|")

  val Numbers: Regex = NumbersPattern.r
  val WordsOrNumbersForward: Regex = s"$NumbersPattern|$WordsPattern".r
  val WordsOrNumbersReverse: Regex =
    s"$NumbersPattern|${WordsPattern.reverse}".r

}
