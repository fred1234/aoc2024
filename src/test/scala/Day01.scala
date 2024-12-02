// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import Day01.parseLine
import Day01.puzzleOne
import Day01.puzzleTwo

class Day01Test extends munit.FunSuite {

  val smallInput = List(
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3"
  )

  test("one line of input should be parsed correctly") {
    val obtained: (Int, Int) = parseLine("3   4")
    val expected = (3, 4)
    assertEquals(obtained, expected)
  }

  test("the small input should be parsed correctly") {
    val obtained: (List[Int], List[Int]) = smallInput.unzip(parseLine)
    val expected = (List(3, 4, 2, 1, 3, 3), List(4, 3, 5, 3, 9, 3))
    assertEquals(obtained, expected)
  }

  test("the small input should solve part1") {
    val (left: List[Int], right: List[Int]) = smallInput.unzip(parseLine)
    val obtained = puzzleOne(left, right)
    val expected = 11
    assertEquals(obtained, expected)
  }

  test("the small input should solve part2") {
    val (left: List[Int], right: List[Int]) = smallInput.unzip(parseLine)
    val obtained = puzzleTwo(left, right)
    val expected = 31
    assertEquals(obtained, expected)
  }
}
