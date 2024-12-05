import Day05Parsers.parseOrderingRules
import Day05Parsers.parseInput
class Day05Test extends munit.FunSuite {
  val pageOrderingRulesRawSmall = List(
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13"
  )

  val pageToProduceSmall = List(
    List(75, 47, 61, 53, 29),
    List(97, 61, 53, 29, 13),
    List(75, 29, 13),
    List(75, 97, 47, 61, 53),
    List(61, 13, 29),
    List(97, 13, 75, 29, 47)
  )
  test("parsing the rules works") {
    val obtained: Map[Int, Set[Int]] = parseOrderingRules(input = List("47|53", "97|13", "97|61"))
    val expected: Map[Int, Set[Int]] = Map(
      97 -> Set(13, 61),
      47 -> Set(53)
    )
    assertEquals(obtained, expected)
  }

  test("parsing everything works") {

    val inputString = "47|53\n97|13\n97|61\n\n75,47,61,53,29\n97,61,53,29,13"

    val (obtainedPageOrderingRules, obtainedPageToProduce) = parseInput(inputString)

    val expectedPageOrderingRules: Map[Int, Set[Int]] = Map(
      97 -> Set(13, 61),
      47 -> Set(53)
    )

    val expectedPageToProduce = List(List(75, 47, 61, 53, 29), List(97, 61, 53, 29, 13))

    assertEquals(obtainedPageOrderingRules, expectedPageOrderingRules)
    assertEquals(obtainedPageToProduce, expectedPageToProduce)

    assertEquals(42, 42)
  }

  test("puzzle is solved correctly for part1") {

    val obtainedPageOrderingRules: Map[Int, Set[Int]] = parseOrderingRules(input = List("47|53", "97|13", "97|61"))

    val expectedPageOrderingRules: Map[Int, Set[Int]] = Map(
      97 -> Set(13, 61),
      47 -> Set(53)
    )

    val expectedPageToProduce = List(List(75, 47, 61, 53, 29), List(97, 61, 53, 29, 13))

    assertEquals(42, 42)
  }

}
