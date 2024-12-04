def solutionOne(input: String): Long = {
  val re = """mul\((\d+{3}),(\d+{3})\)""".r

  re.findAllIn(input)
    .map {
      case s"mul(${first},${last})" => first.toLong * last.toLong
      case _                        => ???
    }
    .sum
}

def solutionTwo(input: String): Long = {
  val re = """do\(\)|don't\(\)|mul\((\d+{3}),(\d+{3})\)""".r
  case class Acc(enabled: Boolean = true, value: Long = 0L)
  val result2: Acc = re
    .findAllIn(input)
    .foldLeft(z = Acc()) {
      case (a @ Acc(true, v), s"mul(${first},${last})")  => a.copy(value = v + first.toLong * last.toLong)
      case (a @ Acc(false, v), s"mul(${first},${last})") => a // case (a, _)
      case (a, "do()")                                   => a.copy(enabled = true)
      case (a, "don't()")                                => a.copy(enabled = false)
      case _                                             => ???
    }
  result2.value
}

object Day03 {

  def main(args: Array[String]): Unit = {
    val raw: String = os.read(os.pwd / "src" / "main" / "resources" / "day03.txt")

    val result1 = solutionOne(raw)
    println(result1)

    val result2 = solutionTwo(raw)
    println(result2)

  }

}
