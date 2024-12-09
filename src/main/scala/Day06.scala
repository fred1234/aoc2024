object Day06 {

  trait PuzzleObject

  enum Type(val symbol: Char) extends PuzzleObject:
    case Obstacle extends Type('#')
    case Empty extends Type('.')
    case Visited extends Type('X')

  enum Player(val symbol: Char) extends PuzzleObject:
    case PlayerUP extends Player('^')
    case PlayerRIGHT extends Player('>')
    case PlayerDOWN extends Player('v')
    case PlayerLEFT extends Player('<')

  type Puzzle = Vector[Vector[PuzzleObject]]

  def main(args: Array[String]): Unit = {
    val raw: Vector[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day06.txt").toVector
    val puzzle = parsePuzzle(raw)

    // part1
    val result1 = play(puzzle)
    println(result1)

    // part2
    println(part2(puzzle))
  }

  def part2(puzzle: Puzzle): Int = {
    putObstacles(puzzle).count(hasLoop)
  }

  def putObstacles(puzzle: Puzzle): List[Puzzle] = {
    val height = puzzle.size
    val width = puzzle(0).size

    (for {
      y <- puzzle.indices
      x <- puzzle(0).indices
      if (puzzle(y)(x) == Type.Empty)
    } yield {
      puzzle.updated(y, puzzle(y).updated(x, Type.Obstacle))
    }).toList

  }

  def hasLoop(puzzle: Puzzle): Boolean = {
    case class PointDirection(x: Int, y: Int, dir: Player)
    val (x, y) = findPlayer(puzzle) // do it once

    def hasLoop(puzzle: Puzzle, posX: Int, posY: Int, directions: Set[PointDirection] = Set()): Boolean = {

      val player: Player = getPlayer(posX, posY, puzzle)

      val (nx, ny) = nextPosition(posX, posY, player)
      val newDir = PointDirection(nx, ny, player)

      // exit conditions
      if (!inBounds(nx, ny, puzzle)) return false
      if (directions.contains(newDir)) return true // we have a loop

      puzzle(ny)(nx) match
        case Type.Obstacle => hasLoop(changeDirection(puzzle, player), posX, posY, directions = directions)
        case Type.Empty | Type.Visited =>
          hasLoop(movePlayer(puzzle, posX, posY, nx, ny, player), nx, ny, directions + newDir)

    }

    hasLoop(puzzle, x, y)

  }

  def play(puzzle: Puzzle): Int = {

    val (x, y) = findPlayer(puzzle) // do it once

    def play(puzzle: Puzzle, posX: Int, posY: Int, steps: Int): Int = {

      val player: Player = getPlayer(posX, posY, puzzle)
      val (nx, ny) = nextPosition(posX, posY, player)

      // exit condition
      if (!inBounds(nx, ny, puzzle)) return steps + 1

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
    def updatePlayer(p: Puzzle, newDir: Player): Puzzle = {
      val (x, y) = findPlayer(p) // could be passed as an argument
      p.updated(y, p(y).updated(x, newDir))
    }

    val newDir = player match
      case Player.PlayerUP    => Player.PlayerRIGHT
      case Player.PlayerRIGHT => Player.PlayerDOWN
      case Player.PlayerDOWN  => Player.PlayerLEFT
      case Player.PlayerLEFT  => Player.PlayerUP
    updatePlayer(p, newDir)
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

  def inBounds(x: Int, y: Int, puzzle: Puzzle): Boolean = {
    val height = puzzle.size
    val width = puzzle(0).size
    x >= 0 && y >= 0 && x < width && y < height
  }

  def getPlayer(posX: Int, posY: Int, puzzle: Puzzle): Player = {
    puzzle(posY)(posX) match
      case s: Player => s
      case Type =>
        ???
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

  def parsePuzzle(raw: Vector[String]) = {
    raw.map(_.map(_ match {
      case '#' => Type.Obstacle
      case '.' => Type.Empty
      case 'X' => Type.Visited
      case '^' => Player.PlayerUP
      case '<' => Player.PlayerLEFT
      case '>' => Player.PlayerRIGHT
      case 'v' => Player.PlayerDOWN
    }).toVector)
  }
}
