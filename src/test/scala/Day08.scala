import Day08.parse
import Day08.solvePart1
import Day08.solvePart2
class Day08Test extends munit.FunSuite {

  val smallInput: List[String] = List(
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
  )

  test("part1 works with small Input") {
    val input = parse(smallInput)
    val height = smallInput.size
    val width = smallInput(0).size

    val obtained = solvePart1(input, height, width)
    val expected = 14
    assertEquals(obtained, expected)
  }

  test("part2 works with small Input") {
    val input = parse(smallInput)
    val height = smallInput.size
    val width = smallInput(0).size

    val obtained = solvePart2(input, height, width)
    val expected = 34
    assertEquals(obtained, expected)
  }

}
