import Day02.parseLine
import Day02.pairs
import Day02.increases
import Day02.decreases
import Day02.inBounds
import Day02.safe
import Day02.tolerate

class Day02Test extends munit.FunSuite {

  val smallInput: List[List[Int]] = List(
    List(7, 6, 4, 2, 1),
    List(1, 2, 7, 8, 9),
    List(9, 7, 6, 2, 1),
    List(1, 3, 2, 4, 5),
    List(8, 6, 4, 4, 1),
    List(1, 3, 6, 7, 9)
  )

  test("one line of input should be parsed correctly") {
    val obtained: List[Int] = parseLine("7 6 4 2 1")
    val expected = List(7, 6, 4, 2, 1)
    assertEquals(obtained, expected)
  }

  test("pairing a list works") {
    val obtained = pairs(List(1, 2, 3))
    val expected = List((1, 2), (2, 3))
    assertEquals(obtained, expected)
  }

  test("checking for strict increasing") {
    val inputOK = pairs(List(1, 2, 3))
    assert(increases(inputOK))

    val inputNOK = pairs(List(1, -1, 2, 3))
    assert(!increases(inputNOK))

  }

  test("checking for strict decreasing") {
    val inputOK = pairs(List(3, 2, 1))
    assert(decreases(inputOK))

    val inputNOK = pairs(List(1, -1, -2, 3))
    assert(!decreases(inputNOK))

  }

  test("checking for inBound") {
    val inputOK = pairs(List(1, 2, 4, 7))
    assert(inBounds(inputOK))

    val inputNOK = pairs(List(1, 2, 9, 7))
    assert(!inBounds(inputNOK))

  }

  test("safety for small input") {
    val obtained: List[Boolean] = smallInput.map(safe)

    val expected = List(true, false, false, false, false, true)
    assertEquals(obtained, expected)
    assertEquals(2, smallInput.count(safe))

  }
  test("safety with tolerance for small input") {

    val obtained: List[Boolean] = smallInput.map(tolerate)

    val expected = List(true, false, false, true, true, true)
    assertEquals(obtained, expected)
    assertEquals(4, smallInput.count(tolerate))

  }
}
