object Day01 {

  def main(args: Array[String]): Unit = {

    val raw: List[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day01.txt").toList
    val firstList = raw.map(line => line.split("   ").apply(0).toInt).sorted
    val secondList = raw.map(line => line.split("   ").apply(1).toInt).sorted

    // part1
    val result = firstList.zip(secondList).map((l, r) => math.abs(l - r)).sum
    println(result)

    // part2
    val lookup = secondList.groupBy(identity).mapValues(_.length)
    val result2 = firstList.map(value => value * lookup.getOrElse(value, 0)).sum
    println(result2)

  }
}
