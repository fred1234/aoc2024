// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import Day04Part01.solvePart1
import Day04Part01.getHorizontalLines
import Day04Part01.getVerticalLines
import Day04Part01.getDiagonalLines
import Day04Part01.generateAllIndices
import Day04Part01.getLineFromIndices
import Day04Part01.countXMAS
import Day04Part01.countXmasLeftToRight
import Day04Part02.solvePart2
import Day04Part02.isXMAS

class Day04TestPart1 extends munit.FunSuite {

  val superSmallInputPart1 = Array(
    Array('.', '.', 'X', '.', '.', '.'),
    Array('.', 'S', 'A', 'M', 'X', '.'),
    Array('.', 'A', '.', '.', 'A', '.'),
    Array('X', 'M', 'A', 'S', '.', 'S'),
    Array('.', 'X', '.', '.', '.', '.')
  )

  val smallInputPart1: Array[Array[Char]] = Array(
    Array('M', 'M', 'M', 'S', 'X', 'X', 'M', 'A', 'S', 'M'),
    Array('M', 'S', 'A', 'M', 'X', 'M', 'S', 'M', 'S', 'A'),
    Array('A', 'M', 'X', 'S', 'X', 'M', 'A', 'A', 'M', 'M'),
    Array('M', 'S', 'A', 'M', 'A', 'S', 'M', 'S', 'M', 'X'),
    Array('X', 'M', 'A', 'S', 'A', 'M', 'X', 'A', 'M', 'M'),
    Array('X', 'X', 'A', 'M', 'M', 'X', 'X', 'A', 'M', 'A'),
    Array('S', 'M', 'S', 'M', 'S', 'A', 'S', 'X', 'S', 'S'),
    Array('S', 'A', 'X', 'A', 'M', 'A', 'S', 'A', 'A', 'A'),
    Array('M', 'A', 'M', 'M', 'M', 'X', 'M', 'M', 'M', 'M'),
    Array('M', 'X', 'M', 'X', 'A', 'X', 'M', 'A', 'S', 'X')
  )

  test("solve the whole puzzle") {
    assertEquals(solvePart1(superSmallInputPart1), expected = 4L)
    assertEquals(solvePart1(smallInputPart1), expected = 18L)
  }

  test("gets all horizontal lines correctly") {
    val obtained = getHorizontalLines(smallInputPart1)
    val expected = List(
      "MMMSXXMASM",
      "MSAMXMSMSA",
      "AMXSXMAAMM",
      "MSAMASMSMX",
      "XMASAMXAMM",
      "XXAMMXXAMA",
      "SMSMSASXSS",
      "SAXAMASAAA",
      "MAMMMXMMMM",
      "MXMXAXMASX"
    )
    assertEquals(obtained, expected)
  }

  test("gets all vertical lines correctly") {
    val obtained = getVerticalLines(smallInputPart1)
    val expected = List(
      "MMAMXXSSMM",
      "MSMSMXMAAX",
      "MAXAAASXMM",
      "SMSMSMMAMX",
      "XXXAAMSMMA",
      "XMMSMXAAXX",
      "MSAMXXSSMM",
      "AMASAAXAMA",
      "SSMMMMSAMS",
      "MAMXMASAMX"
    )

    assertEquals(obtained, expected)
  }

  test("gets all diagonal lines correctly") {
    val obtained = getDiagonalLines(superSmallInputPart1)
    val expected = List(
      ".S.S.",
      ".",
      ".A...",
      "XX",
      ".M.",
      ".AA.",
      "XMAS",
      ".X.",
      "..",
      ".",
      ".M.M.",
      ".",
      "XA.X.",
      "..",
      ".SX",
      "XAA.",
      ".SA.",
      "...",
      ".S",
      "."
    )
    assertEquals(obtained, expected)
  }

  test("generate all indices for diagonal lines") {

    val xxxs = Array(
      Array('a', 'b', 'c', 'd'),
      Array('e', 'f', 'g', 'h'),
      Array('i', 'j', 'k', 'l')
    )
    val obtained = generateAllIndices(height = xxxs.size, width = xxxs(0).size)
    val expected = List(
      IndexedSeq((0, 0), (1, 1), (2, 2)),
      IndexedSeq((0, 3)),
      IndexedSeq((1, 0), (2, 1)),
      IndexedSeq((0, 2), (1, 3)),
      IndexedSeq((0, 1), (1, 2), (2, 3)),
      IndexedSeq((2, 0))
    )
    assertEquals(obtained, expected)
  }

  test("get all lines from given indices") {
    val xxxs = Array(
      Array('a', 'b', 'c', 'd'),
      Array('e', 'f', 'g', 'h'),
      Array('i', 'j', 'k', 'l')
    )
    val obtained1 = getLineFromIndices(IndexedSeq((0, 0), (1, 1), (2, 2)), xxxs)
    assertEquals(obtained1, expected = "afk")
    val obtained2 = getLineFromIndices(IndexedSeq((0, 1), (1, 2), (2, 3)), xxxs)
    assertEquals(obtained2, expected = "bgl")
  }

  test("count XMAS per Line in both directions") {
    assertEquals(obtained = countXMAS("XMAS"), 1L)
    assertEquals(obtained = countXMAS("XMASlol"), 1L)
    assertEquals(obtained = countXMAS("lolXMAS"), 1L)
    assertEquals(obtained = countXMAS("XMAS-XMAS"), 2L)
    assertEquals(obtained = countXMAS("XMAS-LOL-XMAS"), 2L)
    assertEquals(obtained = countXMAS("lolXMAS-LOL-XMASlol"), 2L)
    assertEquals(obtained = countXMAS("SAMX"), 1L)
    assertEquals(obtained = countXMAS("SAMXMAS"), 2L)
    assertEquals(obtained = countXMAS("SAMXXMAS"), 2L)
    assertEquals(obtained = countXMAS("---XMAS====SAMX"), 2L)
  }

  test("count XMAS per Line - left to right") {
    assertEquals(obtained = countXmasLeftToRight("XMAS"), 1L)
    assertEquals(obtained = countXmasLeftToRight("XMASlol"), 1L)
    assertEquals(obtained = countXmasLeftToRight("lolXMAS"), 1L)
    assertEquals(obtained = countXmasLeftToRight("XMAS-XMAS"), 2L)
    assertEquals(obtained = countXmasLeftToRight("XMAS-LOL-XMAS"), 2L)
    assertEquals(obtained = countXmasLeftToRight("lolXMAS-LOL-XMASlol"), 2L)
    assertEquals(obtained = countXmasLeftToRight("SAMX"), 0L)
    assertEquals(obtained = countXmasLeftToRight("SAMXMAS"), 1L)
    assertEquals(obtained = countXmasLeftToRight("SAMXXMAS"), 1L)
    assertEquals(obtained = countXmasLeftToRight("---XMAS====SAMX"), 1L)
  }

}

class Day04TestPart2 extends munit.FunSuite {
// 9 times.
  val smallInputPart2 = Array(
    Array('.', 'M', '.', 'S', '.', '.', '.', '.', '.', '.'),
    Array('.', '.', 'A', '.', '.', 'M', 'S', 'M', 'S', '.'),
    Array('.', 'M', '.', 'S', '.', 'M', 'A', 'A', '.', '.'),
    Array('.', '.', 'A', '.', 'A', 'S', 'M', 'S', 'M', '.'),
    Array('.', 'M', '.', 'S', '.', 'M', '.', '.', '.', '.'),
    Array('.', '.', '.', '.', '.', '.', '.', '.', '.', '.'),
    Array('S', '.', 'S', '.', 'S', '.', 'S', '.', 'S', '.'),
    Array('.', 'A', '.', 'A', '.', 'A', '.', 'A', '.', '.'),
    Array('M', '.', 'M', '.', 'M', '.', 'M', '.', 'M', '.'),
    Array('.', '.', '.', '.', '.', '.', '.', '.', '.', '.')
  )

  test("solve the whole puzzle") {
    assertEquals(solvePart2(smallInputPart2), expected = 9L)
  }

  test("finds XMAS") {
    val sam_sam = Array(
      Array('S', '.', 'S'),
      Array('.', 'A', '.'),
      Array('M', '.', 'M')
    )
    assert(isXMAS(sam_sam, 0, 0))

    val sam_mas = Array(
      Array('S', '.', 'M'),
      Array('.', 'A', '.'),
      Array('S', '.', 'M')
    )
    assert(isXMAS(sam_mas, 0, 0))

    val mas_sam = Array(
      Array('M', '.', 'S'),
      Array('.', 'A', '.'),
      Array('M', '.', 'S')
    )
    assert(isXMAS(sam_mas, 0, 0))

    val mas_mas = Array(
      Array('M', '.', 'M'),
      Array('.', 'A', '.'),
      Array('S', '.', 'S')
    )
    assert(isXMAS(mas_mas, 0, 0))

    val mad_mas = Array(
      Array('M', '.', 'M'),
      Array('.', 'A', '.'),
      Array('S', '.', 'D')
    )
    assert(!isXMAS(mad_mas, 0, 0))
  }

}
