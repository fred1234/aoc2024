import Day06.Type
import Day06.Puzzle

object Day0602 {

  enum Type(val symbol: Char):
    case Obstacle extends Type('#')
    case Empty extends Type('.')
    case Visited extends Type('X')
    case PlayerUP extends Type('^')
    case PlayerRIGHT extends Type('>')
    case PlayerDOWN extends Type('v')
    case PlayerLEFT extends Type('<')

  object Type:
    def fromChar(c: Char): Type = c match
      case '#' => Obstacle
      case '.' => Empty
      case 'X' => Visited
      case '^' => PlayerUP
      case '<' => PlayerLEFT
      case '>' => PlayerRIGHT
      case 'v' => PlayerDOWN

  type Puzzle = Vector[Vector[Type]]

  def main(args: Array[String]): Unit = {
    val raw = os.read.lines(os.pwd / "src" / "main" / "resources" / "day06.txt").toVector
    val puzzle: Puzzle = raw.map(_.map(Type.fromChar).toVector)

    println(getString(puzzle))
    val result = play(puzzle)
    println(result)
  }

  def play(puzzle: Puzzle): Int = {
    val height = puzzle.size
    val width = puzzle(0).size

    def withinBounds(x: Int, y: Int): Boolean =
      x >= 0 && y >= 0 && x < width && y < height

    def nextPosition(x: Int, y: Int, player: Type): (Int, Int) = player match
      case Type.PlayerUP    => (x, y - 1)
      case Type.PlayerRIGHT => (x + 1, y)
      case Type.PlayerDOWN  => (x, y + 1)
      case Type.PlayerLEFT  => (x - 1, y)

    def play(puzzle: Puzzle, steps: Int): Int = {
      val (x, y) = findPlayer(puzzle)
      val player = puzzle(y)(x)

      val (nx, ny) = nextPosition(x, y, player)
      if (!withinBounds(nx, ny)) return steps + 1 // Exit condition

      puzzle(ny)(nx) match
        case Type.Obstacle => play(changeDirection(puzzle, player), steps)
        case Type.Empty    => play(movePlayer(puzzle, x, y, nx, ny, player), steps + 1)
        case Type.Visited  => play(movePlayer(puzzle, x, y, nx, ny, player), steps)
    }

    play(puzzle, 0)
  }

  def changeDirection(p: Puzzle, player: Type): Puzzle = {
    val newDir = player match
      case Type.PlayerUP    => Type.PlayerRIGHT
      case Type.PlayerRIGHT => Type.PlayerDOWN
      case Type.PlayerDOWN  => Type.PlayerLEFT
      case Type.PlayerLEFT  => Type.PlayerUP
    updatePlayer(p, newDir)
  }

  def movePlayer(
      p: Puzzle,
      x: Int,
      y: Int,
      nx: Int,
      ny: Int,
      player: Type
  ): Puzzle = {
    val updated = p.updated(y, p(y).updated(x, Type.Visited))
    updated.updated(ny, updated(ny).updated(nx, player))
  }

  def updatePlayer(p: Puzzle, newDir: Type): Puzzle = {
    val (x, y) = findPlayer(p)
    p.updated(y, p(y).updated(x, newDir))
  }

  def getString(p: Puzzle): String = {
    p.map(_.map(_.symbol).mkString).mkString("\n")
  }

  def findPlayer(p: Puzzle): (Int, Int) = {
    p.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.collect {
        case (cell, x)
            if cell
              .isInstanceOf[Type.PlayerUP.type | Type.PlayerRIGHT.type | Type.PlayerDOWN.type | Type.PlayerLEFT.type] =>
          (x, y)
      }
    }.head // Assumes there is always one player
  }
}
