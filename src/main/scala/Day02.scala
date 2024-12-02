object Day02 {

  def solution1(input: List[List[Int]]) = {
    val differences = input.map { level =>
      level
        .sliding(2)
        .toList
        .map { case List(v1, v2) =>
          v1 - v2
        }
    }

    println(differences)

    val passed = differences.map(diff => {
      val allPositive = diff.forall(_ > 0)
      val allNegative = diff.forall(_ < 0)

      val allInRange = (diff.map(_.abs).toSet -- Set(0, 1, 2, 3)).size == 0

      if ((allPositive || allNegative) && allInRange) 1 else 0

    })

    passed.sum
  }

  def main(args: Array[String]): Unit = {

    val smallInput: List[List[Int]] = List(
      List(7, 6, 4, 2, 1),
      List(1, 2, 7, 8, 9),
      List(9, 7, 6, 2, 1),
      List(1, 3, 2, 4, 5),
      List(8, 6, 4, 4, 1),
      List(1, 3, 6, 7, 9)
    )

    val raw: List[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day02.txt").toList
    val input: List[List[Int]] = raw.map(_.split(" ").map(_.toInt).toList)

    println(solution1(smallInput))

  }
}
