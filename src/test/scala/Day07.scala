import Day07.parseInput
import Day07.solve
import Day07.add
import Day07.mul
import Day07.concat
class Day07Test extends munit.FunSuite {

  val smallInput = List(
    "1019: 10 19",
    "3267: 81 40 27"
  )

  test("parsing the lines works") {
    val obtained: List[(Long, List[Long])] = parseInput(List("21037: 9 7 18 13"))
    val expected: List[(Long, List[Long])] = List((21037L, List(9L, 7L, 18L, 13L)))
    assertEquals(obtained, expected)
  }

  test("part1 works with small Input") {
    val input: List[(Long, List[Long])] = parseInput(smallInput)
    val obtained: Long = solve(input, Set(add, mul))
    val expected: Long = 3267L
    assertEquals(obtained, expected)
  }

  test("part2 works with small Input") {
    val input: List[(Long, List[Long])] = parseInput(smallInput)
    val obtained: Long = solve(input, Set(add, mul, concat))
    val expected: Long = 4286L
    assertEquals(obtained, expected)
  }

}
