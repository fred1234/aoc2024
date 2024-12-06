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

    println(getString(puzzle))
    val result = play(puzzle)
    println(result)

  }

  def play(puzzle: Puzzle): Int = {
    val height = puzzle.size
    val width = puzzle(0).size

    def play(puzzle: Puzzle, numStep: Int): Int = {
      val (x, y) = findPlayer(puzzle)
      val player = puzzle(y)(x)

      // ending Conditions
      player match
        case Type.PlayerUP if y == 0                               => numStep + 1
        case Type.PlayerUP if puzzle(y - 1)(x) == Type.Obstacle    => play(setRight(puzzle), numStep)
        case Type.PlayerUP if puzzle(y - 1)(x) == Type.Empty       => play(goUp(puzzle), numStep + 1)
        case Type.PlayerUP if puzzle(y - 1)(x) == Type.Visited     => play(goUp(puzzle), numStep)
        case Type.PlayerRIGHT if x == width - 1                    => numStep + 1
        case Type.PlayerRIGHT if puzzle(y)(x + 1) == Type.Obstacle => play(setDown(puzzle), numStep)
        case Type.PlayerRIGHT if puzzle(y)(x + 1) == Type.Empty    => play(goRight(puzzle), numStep + 1)
        case Type.PlayerRIGHT if puzzle(y)(x + 1) == Type.Visited  => play(goRight(puzzle), numStep)
        case Type.PlayerDOWN if y == height - 1                    => numStep + 1
        case Type.PlayerDOWN if puzzle(y + 1)(x) == Type.Obstacle  => play(setLeft(puzzle), numStep)
        case Type.PlayerDOWN if puzzle(y + 1)(x) == Type.Empty     => play(goDown(puzzle), numStep + 1)
        case Type.PlayerDOWN if puzzle(y + 1)(x) == Type.Visited   => play(goDown(puzzle), numStep)
        case Type.PlayerLEFT if x == 0                             => numStep + 1
        case Type.PlayerLEFT if puzzle(y)(x - 1) == Type.Obstacle  => play(setUp(puzzle), numStep)
        case Type.PlayerLEFT if puzzle(y)(x - 1) == Type.Empty     => play(goLeft(puzzle), numStep + 1)
        case Type.PlayerLEFT if puzzle(y)(x - 1) == Type.Visited   => play(goLeft(puzzle), numStep)

    }

    play(puzzle, 0)

  }

  def setRight(p: Puzzle): Puzzle = { setDir(p, Type.PlayerRIGHT) }
  def setLeft(p: Puzzle): Puzzle = { setDir(p, Type.PlayerLEFT) }
  def setDown(p: Puzzle): Puzzle = { setDir(p, Type.PlayerDOWN) }
  def setUp(p: Puzzle): Puzzle = { setDir(p, Type.PlayerUP) }
  def setDir(p: Puzzle, dir: Type) = {
    val (x, y) = findPlayer(p)
    p.updated(y, p(y).updated(x, dir))
  }

  def goUp(p: Puzzle): Puzzle = {
    val (x, y) = findPlayer(p)
    val n = p.updated(y, p(y).updated(x, Type.Visited))
    n.updated(y - 1, n(y - 1).updated(x, Type.PlayerUP))
  }
  def goDown(p: Puzzle): Puzzle = {
    val (x, y) = findPlayer(p)
    val n = p.updated(y, p(y).updated(x, Type.Visited))
    n.updated(y + 1, n(y + 1).updated(x, Type.PlayerDOWN))
  }
  def goLeft(p: Puzzle): Puzzle = {
    val (x, y) = findPlayer(p)
    val n = p.updated(y, p(y).updated(x, Type.Visited))
    n.updated(y, n(y).updated(x - 1, Type.PlayerLEFT))
  }
  def goRight(p: Puzzle): Puzzle = {
    val (x, y) = findPlayer(p)
    val n = p.updated(y, p(y).updated(x, Type.Visited))
    n.updated(y, n(y).updated(x + 1, Type.PlayerRIGHT))
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
