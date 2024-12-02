object Day02 {

  def parseLine(l: String): List[Int] = l.split(" ").map(_.toInt).toList

  def pairs(level: List[Int]): List[(Int, Int)] = level.zip(level.tail)
  def increases(input: List[(Int, Int)]): Boolean = input.forall(_ < _)
  def decreases(input: List[(Int, Int)]): Boolean = input.forall(_ > _)
  def inBounds(input: List[(Int, Int)]): Boolean = {
    val diffs = input.map((v1, v2) => math.abs(v1 - v2))
    (diffs.toSet -- Set(0, 1, 2, 3)).isEmpty
  }
  def safe(input: List[Int]): Boolean = {
    val paired = pairs(input)
    (increases(paired) || decreases(paired)) && inBounds(paired)
  }

  def tolerate(input: List[Int]): Boolean = {
    input.indices.exists(index => safe(input.patch(index, Nil, 1)))
  }

  def main(args: Array[String]): Unit = {

    val raw: List[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day02.txt").toList
    val input = raw.map(parseLine)

    val solution1 = input.count(safe)
    println(solution1)

    val solution2 = input.count(tolerate)
    println(solution2)

  }
}
