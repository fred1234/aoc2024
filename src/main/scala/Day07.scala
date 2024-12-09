object Day07 {

  def main(args: Array[String]): Unit = {
    val raw = os.read.lines(os.pwd / "src" / "main" / "resources" / "day07.txt").toList
    val input: List[(Long, List[Long])] = parseInput(raw)

    // part1
    println(solve(input, Set(add, mul)))

    // part2
    println(solve(input, Set(add, mul, concat)))

  }

  trait Operator:
    def apply(left: Long, right: Long): Long

  val add: Operator = (left: Long, right: Long) => left + right
  val mul: Operator = (left: Long, right: Long) => left * right
  val concat: Operator = (left: Long, right: Long) => s"${left}${right}".toLong

  def solve(input: List[(Long, List[Long])], operators: Set[Operator]) = {

    def isSolvable(
        result: Long,
        operands: List[Long],
        operators: Set[Operator],
        acc: Long = 0L
    ): Boolean = {
      if (acc > result) false // early exit
      else
        operands match {
          case operand :: rest =>
            operators.exists(operator => isSolvable(result, operands = rest, operators, operator(acc, operand)))
          case Nil => result == acc // exit condition
        }

    }

    val valides: List[(Long, List[Long])] = input.filter { case (result, operands) =>
      isSolvable(result, operands, operators)
    }

    valides.map { (a, _) => a }.sum

  }

  def parseInput(input: List[String]): List[(Long, List[Long])] = {
    input.map { case s"${result}: ${rest}" =>
      (result.toLong, rest.split("""\s+""").map(_.toLong).toList)
    }
  }

}
