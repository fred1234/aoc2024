import Day04Part01.solvePart1
import Day04Part02.solvePart2
object Day04 {

  def main(args: Array[String]): Unit = {
    val raw: Array[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day04.txt").toArray
    val input: Array[Array[Char]] = raw.map(_.toArray)
    println(solvePart1(input))
    println(solvePart2(input))
  }

}

object Day04Part01 {
  def solvePart1(input: Array[Array[Char]]): Long = {
    val allWords: List[String] =
      getHorizontalLines(input) ++
        getVerticalLines(input) ++
        getDiagonalLines(input)

    allWords.map(countXMAS).sum
  }

  def getHorizontalLines(puzzleInput: Array[Array[Char]]): List[String] =
    puzzleInput.map(_.mkString).toList

  def getVerticalLines(puzzleInput: Array[Array[Char]]): List[String] =
    puzzleInput.transpose.map(_.mkString).toList

  def getDiagonalLines(puzzleInput: Array[Array[Char]]): List[String] = {
    val indices = generateAllIndices(height = puzzleInput.size, width = puzzleInput(0).size)

    val wordsNegSlope: List[String] = indices.map(getLineFromIndices(_, puzzleInput))

    // when we reverse the array we get the diagonals with positive slope :)
    val wordsPosSlope: List[String] = indices.map(getLineFromIndices(_, puzzleInput.reverse))

    wordsNegSlope ++ wordsPosSlope
  }

  def generateAllIndices(height: Int, width: Int): List[IndexedSeq[(Int, Int)]] = {

    val allStartsFromCol0 = for r <- 0 until height yield (r, 0)
    val allStartsFromRow1 = for c <- 0 until width yield (0, c)
    val allStarts = (allStartsFromCol0 ++ allStartsFromRow1).toSet

    allStarts.map { case (startRow: Int, startCol: Int) =>
      for {
        r <- startRow until height
        c <- startCol until width
        if (r - startRow == c - startCol)
      } yield { (r, c) }

    }.toList

  }

  def getLineFromIndices(indices: IndexedSeq[(Int, Int)], puzzleInput: Array[Array[Char]]): String = {
    indices.map((row, col) => puzzleInput(row)(col)).mkString
  }

  def countXMAS(line: String): Long = {
    countXmasLeftToRight(line) + countXmasLeftToRight(line.reverse)
  }

  def countXmasLeftToRight(line: String): Long = {
    // Thanks AoC for the very specific "or even overlapping other words"
    // val pattern = "(?=(X.*?M.*?A.*?S))".r
    // val pattern = "(X.*?M.*?A.*?S)".r
    val pattern = ".*?(XMAS)".r

    pattern.findAllMatchIn(line).map(_.group(1)).size
  }
}
object Day04Part02 {

  def solvePart2(input: Array[Array[Char]]): Long = {
    val height = input.size
    val width = input(0).size

    val results2 = for {
      r <- 0 until height - 2
      c <- 0 until width - 2
      if input(r)(c) == 'M' || input(r)(c) == 'S'
    } yield {
      isXMAS(input, r, c)
    }
    results2.count(e => e)

  }

  def isXMAS(input: Array[Array[Char]], row: Int, col: Int): Boolean = {

    /** | --- | --- | --- | --- |
      * |:----|:----|:----|:----|
      * | r/c | +0  | +1  | +2  |
      * | --- | --- | --- | --- |
      * | +0  | 'a' | 'b' | 'c' |
      * | +1  | 'd' | 'e' | 'f' |
      * | +2  | 'g' | 'h' | 'i' |
      * | --- | --- | --- | --- |
      */

    val backslash = s"${input(row)(col)}${input(row + 1)(col + 1)}${input(row + 2)(col + 2)}"
    val slash = s"${input(row + 2)(col)}${input(row + 1)(col + 1)}${input(row + 0)(col + 2)}"

    (backslash == "SAM" || backslash == "MAS") && (slash == "SAM" || slash == "MAS")
  }

}
