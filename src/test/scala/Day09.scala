import Day09.part1
import Day09.part2
class Day09Test extends munit.FunSuite {

  val raw = "2333133121414131402"

  test("part1 works with small Input") {
    val obtained: Long = part1(raw)
    val expected: Long = 1928L
    assertEquals(obtained, expected)
  }

  test("part2 works with small Input") {
    val obtained: Long = part2(raw)
    val expected: Long = 2858L
    assertEquals(obtained, expected)
  }

}
