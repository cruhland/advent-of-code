package net.cruhland.aoc.y2024

object Day05 {

  def solution1(input: String): Int = {
    // Parse input into data structures (ordering rules, possible updates)
    // Keep only the updates that are valid according to the rules
    // Add up the middle page numbers of the valid updates
    ???
  }

  class OrderingRules[A](rules: (A, A)*) {
    def validate(update: Seq[A]): Boolean = {
      rules.isEmpty
    }
  }

}
