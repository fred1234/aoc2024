import Day05Parsers.parseInput

object Day05Parsers {

  def parseInput(input: String): (Map[Int, Set[Int]], List[List[Int]]) = {
    val Array(pageOrderingRulesAsString, pageToProduceRaw) = input.split("\n\n")

    val pageOrderingRulesAsList = pageOrderingRulesAsString.split("\n").toList
    val pageOrderingRules: Map[Int, Set[Int]] = parseOrderingRules(pageOrderingRulesAsList)

    val pageToProduce = pageToProduceRaw.split("\n").map(_.split(",").toList.map(_.toInt)).toList

    (pageOrderingRules, pageToProduce)

  }

  def parseOrderingRules(input: List[String]): Map[Int, Set[Int]] = {
    val splitted: List[(Int, Int)] = input.map(_.split("\\|")).map(a => (a(0).toInt, a(1).toInt))
    splitted
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)
      .toMap
  }

}
object Day05 {

  def isValid(update: List[Int], pageOrderingRules: Map[Int, Set[Int]]): Boolean = {
    val updateReverse = update.reverse

    def isValidAcc(updateReverse: List[Int], acc: Boolean): Boolean = {
      updateReverse match {
        case head :: next =>
          val isValid = pageOrderingRules.getOrElse(head, Set()).intersect(next.toSet).isEmpty
          isValidAcc(next, acc && isValid)
        case Nil =>
          acc
      }
    }

    isValidAcc(updateReverse, true)

  }

  def repair(update: List[Int], pageOrderingRules: Map[Int, Set[Int]]): List[Int] = {

    val updateReverse = update.reverse

    def repair(updates: List[Int], acc: List[Int] = List.empty): List[Int] = updates match {
      case head :: next => {
        val isValid = pageOrderingRules.getOrElse(head, Set()).intersect(next.toSet).isEmpty
        if (isValid) {
          repair(next, acc :+ head)
        } else {
          val violatesSet = pageOrderingRules(head).intersect(next.toSet)
          // find out which one violates first in the update
          val beforeHead = updates.filter(violatesSet.contains).last
          // the head has to be inserted before this element

          val newUpdates = updates.flatMap {
            case `beforeHead` => List(beforeHead, head)
            case `head`       => List()
            case x            => List(x)
          }
          repair(newUpdates ++ acc, List.empty)
        }

      }
      case Nil => acc
    }

    repair(updateReverse).reverse

  }

  def middleSum(input: List[List[Int]]): Int = input.map { update => update(update.size / 2) }.sum

  def solvePart1(pageOrderingRules: Map[Int, Set[Int]], pageToProduce: List[List[Int]]) = {
    val valides = pageToProduce.filter(isValid(_, pageOrderingRules))
    middleSum(valides)
  }

  def solvePart2(pageOrderingRules: Map[Int, Set[Int]], pageToProduce: List[List[Int]]) = {
    val valides = pageToProduce.filter(isValid(_, pageOrderingRules))
    val notValides: List[List[Int]] = (pageToProduce.toSet -- valides.toSet).toList
    val repaired: List[List[Int]] = notValides.map(update => repair(update.reverse, pageOrderingRules))
    middleSum(repaired)
  }

  def main(args: Array[String]): Unit = {
    val raw = os.read(os.pwd / "src" / "main" / "resources" / "day05.txt")
    val (pageOrderingRules, pageToProduce) = parseInput(raw)

    println(solvePart1(pageOrderingRules, pageToProduce))
    println(solvePart2(pageOrderingRules, pageToProduce))
  }

}
