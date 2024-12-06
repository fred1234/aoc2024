import Day06.Type
import Day06.Puzzle
object Day06 {

  enum Type(val symbol: Char):
    case Obstacle extends Type('#')
    case Empty extends Type('.')
    case Visited extends Type('X')

  enum Player(val symbol: Char):
    case PlayerUP extends Player('^')
    case PlayerRIGHT extends Player('>')
    case PlayerDOWN extends Player('v')
    case PlayerLEFT extends Player('<')

  type Puzzle = Vector[Vector[Type | Player]]

  def main(args: Array[String]): Unit = {
    val raw = os.read.lines(os.pwd / "src" / "main" / "resources" / "day06.txt").toVector

    val puzzle: Puzzle = raw.map(_.map(_ match {
      case '#' => Type.Obstacle
      case '.' => Type.Empty
      case 'X' => Type.Visited
      case '^' => Player.PlayerUP
      case '<' => Player.PlayerLEFT
      case '>' => Player.PlayerRIGHT
      case 'v' => Player.PlayerDOWN
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
      val player = puzzle(posY)(posX) match
        case Type      => ???
        case s: Player => s

      val (nx, ny) = nextPosition(posX, posY, player)
      if (!inBounds(nx, ny)) return steps + 1

      puzzle(ny)(nx) match
        case Type.Obstacle => play(changeDirection(puzzle, player), posX, posY, steps)
        case Type.Empty    => play(movePlayer(puzzle, posX, posY, nx, ny, player), nx, ny, steps + 1)
        case Type.Visited  => play(movePlayer(puzzle, posX, posY, nx, ny, player), nx, ny, steps)
    }
    play(puzzle, x, y, 0)
  }

  def nextPosition(x: Int, y: Int, player: Player): (Int, Int) = player match
    case Player.PlayerUP    => (x, y - 1)
    case Player.PlayerRIGHT => (x + 1, y)
    case Player.PlayerDOWN  => (x, y + 1)
    case Player.PlayerLEFT  => (x - 1, y)

  def changeDirection(p: Puzzle, player: Player): Puzzle = {
    val newDir = player match
      case Player.PlayerUP    => Player.PlayerRIGHT
      case Player.PlayerRIGHT => Player.PlayerDOWN
      case Player.PlayerDOWN  => Player.PlayerLEFT
      case Player.PlayerLEFT  => Player.PlayerUP
    updatePlayer(p, newDir)
  }

  def updatePlayer(p: Puzzle, newDir: Player): Puzzle = {
    val (x, y) = findPlayer(p)
    p.updated(y, p(y).updated(x, newDir))
  }

  def movePlayer(
      p: Puzzle,
      x: Int,
      y: Int,
      nx: Int,
      ny: Int,
      player: Player
  ): Puzzle = {
    val updated = p.updated(y, p(y).updated(x, Type.Visited))
    updated.updated(ny, updated(ny).updated(nx, player))
  }

  def getString(p: Puzzle): String = {
    p.map(
      _.map(e =>
        e match
          case t: Type   => t.symbol
          case s: Player => s.symbol
      ).mkString
    ).mkString("\n")
  }

  def findPlayer(p: Puzzle) = {
    val x = for {
      (row, y) <- p.zipWithIndex
      (cell, x) <- row.zipWithIndex
    } yield {
      cell match
        case Type.Obstacle      => None
        case Type.Empty         => None
        case Type.Visited       => None
        case Player.PlayerUP    => Some((x, y))
        case Player.PlayerRIGHT => Some((x, y))
        case Player.PlayerDOWN  => Some((x, y))
        case Player.PlayerLEFT  => Some((x, y))
    }
    x.flatten.head // there must be one player
  }
}
