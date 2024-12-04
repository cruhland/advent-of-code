package net.cruhland.aoc.y2024

import scala.util.matching.Regex

object Day03 {

  def solution1(input: String): Int = {
    MulRegex
      .findAllMatchIn(input)
      .map { matchData =>
        matchData
          .subgroups
          .map(_.toInt)
          .product
      }
      .sum
  }

  def solution2(input: String): Int = {
    MulDoRegex
      .findAllMatchIn(input)
      .foldLeft(SolutionState()) { (solutionState, matchData) =>
        matchData.matched match {
          case "don't()" =>
            solutionState.copy(enabled = false)
          case "do()" =>
            solutionState.copy(enabled = true)
          case _ if solutionState.enabled =>
            val mul = matchData
              .subgroups
              .map(_.toInt)
              .product
            solutionState.copy(total = solutionState.total + mul)
          case _ =>
            solutionState
        }
      }
      .total
  }

  def solution2Monadic(input: String): Int = {
    ???
  }

  case class SolutionState(total: Int = 0, enabled: Boolean = true)

  val MulPattern: String = raw"mul\((\d+),(\d+)\)"
  val MulRegex: Regex = raw"$MulPattern".r
  val MulDoRegex: Regex = raw"$MulPattern|don't\(\)|do\(\)".r

}
