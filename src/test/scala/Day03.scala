class Day03Test extends munit.FunSuite {

  val small_part_one = """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""
  val small_part_two = """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""

  test("solution for part 1") {
    val obtained: Long = solutionOne(small_part_one)
    val expected = 161L
    assertEquals(obtained, expected)
  }
  test("solution for part 2") {
    val obtained: Long = solutionTwo(small_part_two)
    val expected = 48L
    assertEquals(obtained, expected)
  }

}
