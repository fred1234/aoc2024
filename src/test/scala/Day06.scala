import Day06.Puzzle
import Day06.Type
import Day06.Player
import Day06.parsePuzzle
import Day06.PuzzleObject
import Day06.play
import Day06.hasLoop
import Day06.part2
class Day06Test extends munit.FunSuite {

  val puzzleToTestWithoutLoop: Puzzle = Vector(
    Vector(Player.PlayerRIGHT, Type.Empty, Type.Obstacle),
    Vector(Type.Empty, Type.Empty, Type.Empty),
    Vector(Type.Empty, Type.Empty, Type.Empty)
  )

  val puzzleToTestWithLoop: Puzzle = Vector(
    Vector(Player.PlayerRIGHT, Type.Empty, Type.Empty, Type.Empty, Type.Empty, Type.Obstacle),
    Vector(Type.Empty, Type.Empty, Type.Obstacle, Type.Empty, Type.Empty, Type.Empty),
    Vector(Type.Empty, Type.Obstacle, Type.Empty, Type.Empty, Type.Empty, Type.Obstacle),
    Vector(Type.Empty, Type.Empty, Type.Empty, Type.Empty, Type.Obstacle, Type.Empty)
  )
  val puzzleToTestWithOneObstacleAwayFromLoop: Puzzle = Vector(
    Vector(Player.PlayerRIGHT, Type.Empty, Type.Empty, Type.Empty, Type.Empty, Type.Obstacle),
    Vector(Type.Empty, Type.Empty, Type.Obstacle, Type.Empty, Type.Empty, Type.Empty),
    Vector(Type.Empty, Type.Obstacle, Type.Empty, Type.Empty, Type.Empty, Type.Obstacle),
    Vector(Type.Empty, Type.Empty, Type.Empty, Type.Empty, Type.Empty, Type.Empty)
  )

  test("parse Puzzle works") {
    val obtained: Vector[Vector[PuzzleObject]] = parsePuzzle(Vector(">...#"))
    val expected = Vector(Vector(Player.PlayerRIGHT, Type.Empty, Type.Empty, Type.Empty, Type.Obstacle))
    assertEquals(obtained, expected)
  }

  test("part1 works with small Input") {
    val obtained: Int = play(puzzleToTestWithoutLoop)
    val expected: Int = 4
    assertEquals(obtained, expected)
  }

  test("a loop can be detected") {
    assert(hasLoop(puzzleToTestWithLoop))
    assert(!hasLoop(puzzle = puzzleToTestWithoutLoop))
  }

  test("part2 works with small Input") {
    val obtained: Int = part2(puzzleToTestWithOneObstacleAwayFromLoop)
    val expected: Int = 1
    assertEquals(obtained, expected)
  }

}
