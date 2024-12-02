object Day01 {

  def parseLine(line: String): (Int, Int) = {
    val splitted = line.split("   ")
    (splitted(0).toInt, splitted(1).toInt)
  }

  def puzzleOne(l: List[Int], r: List[Int]): Int = {
    l.sorted
      .zip(r.sorted)
      .map((l, r) => math.abs(l - r))
      .sum
  }

  def puzzleTwo(l: List[Int], r: List[Int]): Int = {
    val freq = r.groupBy(identity).mapValues(_.length)
    l.map(value => value * freq.getOrElse(value, 0)).sum
  }

  def main(args: Array[String]): Unit = {
    val raw: List[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day01.txt").toList
    val (firstList: List[Int], secondList: List[Int]) = raw.unzip(parseLine)

    println(puzzleOne(firstList, secondList))
    println(puzzleTwo(firstList, secondList))

  }
}
