package net.cruhland.aoc.y2024

import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day06Spec extends AnyFreeSpec with Matchers {

  "solution1" - {

    "guard leaves smallest area" in {
      val input = "^"
      val answer = Day06.solution1(input)
      answer mustBe 1
    }

    "guard leaves after moving one space" in {
      val input =
        """.
          |^
          |""".stripMargin

      val answer = Day06.solution1(input)
      answer mustBe 2
    }

    "extra space behind doesn't count, going north" in {
      val input =
        """^
          |.
          |""".stripMargin

      val answer = Day06.solution1(input)
      answer mustBe 1
    }

    "extra space behind doesn't count, going south" in {
      val input =
        """.
          |V
          |""".stripMargin

      val answer = Day06.solution1(input)
      answer mustBe 1
    }

    "extra space behind doesn't count, going east" in {
      val input = ".>"
      val answer = Day06.solution1(input)
      answer mustBe 1
    }

    "extra space behind doesn't count, going west" in {
      val input = "<."
      val answer = Day06.solution1(input)
      answer mustBe 1
    }

    "extra space on the left doesn't count, going north" in {
      val input = ".^"
      val answer = Day06.solution1(input)
      answer mustBe 1
    }

    "obstacle causes right turn" in {
      val input = ">#"
      val answer = Day06.solution1(input)
      answer mustBe 1
    }

    "obstacle behind guard does not cause turn" in {
      val input = "#>."
      val answer = Day06.solution1(input)
      answer mustBe 2
    }

    "guard continues after turn" in {
      val input =
        """>#
          |..
          |""".stripMargin

      val answer = Day06.solution1(input)
      answer mustBe 2
    }

    "distance before obstacle is included in result" in {
      val input = ">.#"
      val answer = Day06.solution1(input)
      answer mustBe 2
    }

    "count visited cells, not steps" in {
      val input =
        """>.#
          |.#.
          |""".stripMargin

      val answer = Day06.solution1(input)
      answer mustBe 2
    }
  }

}
