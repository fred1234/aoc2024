object Day01 {

  def parseLine(line: String): (Int, Int) = {
    val splitted = line.split("   ")
    (splitted(0).toInt, splitted(1).toInt)
  }

  def main(args: Array[String]): Unit = {
    val raw: List[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day01.txt").toList

    val (firstList: List[Int], secondList: List[Int]) = raw.unzip(parseLine)

    val result = firstList.sorted().zip(secondList.sorted()).map((l, r) => math.abs(l - r)).sum
    println(result)

    val freq = secondList.groupBy(identity).mapValues(_.length)

    val result2 = firstList.map(value => value * freq.getOrElse(value, 0)).sum
    println(result2)

  }
}
