import Day06.Type
import Day06.Puzzle
object Day06 {

  enum Type(val symbol: Char):
    case Obstacle extends Type('#')
    case Empty extends Type('.')
    case Visited extends Type('X')
    case PlayerUP extends Type('^')
    case PlayerRIGHT extends Type('>')
    case PlayerDOWN extends Type('v')
    case PlayerLEFT extends Type('<')

  type Puzzle = Vector[Vector[Type]]

  def main(args: Array[String]): Unit = {
    val raw = os.read.lines(os.pwd / "src" / "main" / "resources" / "day06.txt").toVector

    val puzzle: Puzzle = raw.map(_.map(_ match {
      case '#' => Type.Obstacle
      case '.' => Type.Empty
      case 'X' => Type.Visited
      case '^' => Type.PlayerUP
      case '<' => Type.PlayerLEFT
      case '>' => Type.PlayerRIGHT
      case 'v' => Type.PlayerDOWN
    }).toVector)

    // val puzzle: Puzzle = Vector(
    //   Vector(Type.PlayerRIGHT, Type.Empty, Type.Obstacle),
    //   Vector(Type.Empty, Type.Empty, Type.Empty),
    //   Vector(Type.Empty, Type.Empty, Type.Empty)
    // )

    val result = play(puzzle)
    println(result)

  }

  def play(puzzle: Puzzle): Int = {
    val height = puzzle.size
    val width = puzzle(0).size
    val (x, y) = findPlayer(puzzle) // do it once

    def inBounds(x: Int, y: Int): Boolean =
      x >= 0 && y >= 0 && x < width && y < height

    def play(puzzle: Puzzle, posX: Int, posY: Int, steps: Int): Int = {
      val player = puzzle(posY)(posX)

      val (nx, ny) = nextPosition(posX, posY, player)
      if (!inBounds(nx, ny)) return steps + 1

      puzzle(ny)(nx) match
        case Type.Obstacle => play(changeDirection(puzzle, player), posX, posY, steps)
        case Type.Empty    => play(movePlayer(puzzle, posX, posY, nx, ny, player), nx, ny, steps + 1)
        case Type.Visited  => play(movePlayer(puzzle, posX, posY, nx, ny, player), nx, ny, steps)
    }
    play(puzzle, x, y, 0)
  }

  def nextPosition(x: Int, y: Int, player: Type): (Int, Int) = player match
    case Type.PlayerUP    => (x, y - 1)
    case Type.PlayerRIGHT => (x + 1, y)
    case Type.PlayerDOWN  => (x, y + 1)
    case Type.PlayerLEFT  => (x - 1, y)

  def changeDirection(p: Puzzle, player: Type): Puzzle = {
    val newDir = player match
      case Type.PlayerUP    => Type.PlayerRIGHT
      case Type.PlayerRIGHT => Type.PlayerDOWN
      case Type.PlayerDOWN  => Type.PlayerLEFT
      case Type.PlayerLEFT  => Type.PlayerUP
    updatePlayer(p, newDir)
  }

  def updatePlayer(p: Puzzle, newDir: Type): Puzzle = {
    val (x, y) = findPlayer(p)
    p.updated(y, p(y).updated(x, newDir))
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

  def getString(p: Puzzle): String = {
    p.map(_.map(_.symbol).mkString).mkString("\n")
  }

  def findPlayer(p: Puzzle) = {
    val x = for {
      (row, y) <- p.zipWithIndex
      (cell, x) <- row.zipWithIndex
    } yield {
      cell match
        case Type.Obstacle    => None
        case Type.Empty       => None
        case Type.Visited     => None
        case Type.PlayerUP    => Some((x, y))
        case Type.PlayerRIGHT => Some((x, y))
        case Type.PlayerDOWN  => Some((x, y))
        case Type.PlayerLEFT  => Some((x, y))
    }
    x.flatten.head // there must be one player
  }
}
