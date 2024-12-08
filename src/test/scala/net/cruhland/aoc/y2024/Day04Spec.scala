package net.cruhland.aoc.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Day04Spec extends AnyFreeSpec with Matchers {
  import Day04._

  "Day04.solution1" - {

    "single horizontal word" in {
      val input = "XMAS"
      val answer = solution1(input)
      answer mustBe 1
    }

    "horizontal non-word" in {
      val input = "MAXS"
      val answer = solution1(input)
      answer mustBe 0
    }

    "single horizontal word, with prefix and suffix" in {
      val input = "AXMASA"
      val answer = solution1(input)
      answer mustBe 1
    }

    "single backwards horizontal word" in {
      val input = "SAMX"
      val answer = solution1(input)
      answer mustBe 1
    }

    "single backwards horizontal word, with prefix and suffix" in {
      val input = "MSAMXX"
      val answer = solution1(input)
      answer mustBe 1
    }

    "horizontal words on separate lines" in {
      val input = "XMAS\nSAMX\n"
      val answer = solution1(input)
      answer mustBe 2
    }

    "words do not wrap across lines" in {
      val input = "XMAXM\nASXMA\n"
      val answer = solution1(input)
      answer mustBe 0
    }

    "single vertical word" in {
      val input =
        """X
          |M
          |A
          |S
          |""".stripMargin
      val answer = solution1(input)
      answer mustBe 1
    }

    "vertical non-word" in {
      val input =
        """X
          |M
          |A
          |X
          |""".stripMargin
      val answer = solution1(input)
      answer mustBe 0
    }

    "single backwards vertical word" in {
      val input =
        """S
          |A
          |M
          |X
          |""".stripMargin
      val answer = solution1(input)
      answer mustBe 1
    }

    "one horizontal, one vertical non-overlapping words" in {
      val input =
        """XMAS
          |SXMA
          |SMMA
          |SAMA
          |SSMA
          |""".stripMargin
      val answer = solution1(input)
      answer mustBe 2
    }

    "lower left to upper right diagonal" in {
      val input =
        """XXXS
          |XXAX
          |XMXX
          |XXXX
          |""".stripMargin
      val answer = solution1(input)
      answer mustBe 1
    }

    "upper left to lower right diagonal" in {
      val input =
        """XMMM
          |MMMM
          |MMAM
          |MMMS
          |""".stripMargin
      val answer = solution1(input)
      answer mustBe 1
    }
  }

  "example" in {
    val input =
      """MMMSXXMASM
        |MSAMXMSMSA
        |AMXSXMAAMM
        |MSAMASMSMX
        |XMASAMXAMM
        |XXAMMXXAMA
        |SMSMSASXSS
        |SAXAMASAAA
        |MAMMMXMMMM
        |MXMXAXMASX
        |""".stripMargin
    val answer = solution1(input)
    answer mustBe 18
  }

}
