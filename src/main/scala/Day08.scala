import os.makeDir.all
import Day07.add
import scala.collection.MapView
object Day08 {

  case class Pos(x: Int, y: Int, symbol: Char):
    def -(that: Pos): Pos = copy(x = x - that.x, y = y - that.y)
    def +(that: Pos): Pos = copy(x = x + that.x, y = y + that.y)

  def main(args: Array[String]): Unit = {
    val raw: List[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day08.txt").toList

    val smallInput: List[String] = List(
      "............",
      "........0...",
      ".....0......",
      ".......0....",
      "....0.......",
      "......A.....",
      "............",
      "............",
      "........A...",
      ".........A..",
      "............",
      "............"
    )

    val height = smallInput.size
    val width = smallInput(0).size
    println(height)
    println(width)

    val world: List[Pos] = parse(smallInput)

    println(solvePart1(world, height, width))

  }

  def parse(raw: List[String]): List[Pos] = {
    for {
      y <- raw.indices
      x <- raw(0).indices
    } yield {
      Pos(x, y, raw(y).charAt(x))
    }
  }.toList

  def solvePart1(world: List[Pos], height: Int, width: Int) = {

    val allAntennas: List[Pos] = world.filter(_.symbol != '.')
    val allAntennaCoordinates: Set[(Int, Int)] = allAntennas.map { case Pos(x, y, _) => (x, y) }.toSet
    val groupedByAntenna: Map[Char, Set[Pos]] = allAntennas.groupBy(_.symbol).mapValues(_.toSet).toMap

    println("groupedByAntenna")
    groupedByAntenna.foreach(println)

    val allAntinodes = groupedByAntenna.map { (antenna, listOfPos) =>
      // for a give antenna-type, here we have all the differences of all combinations

      val allDiffs: List[Pos] = listOfPos.toList
        .combinations(2)
        .map {
          case List(first, second) => first - second
          case _                   => ???
        }
        .toList

      // now we collect the possible locations.
      val allAntinodesForAntenna: Set[Pos] = listOfPos.flatMap { pos =>
        allDiffs.flatMap { diff =>

          if ((pos + diff).x == 5 && (pos + diff).y == 4) {
            println("panic")
          }

          val added: Set[Pos] = {
            pos + diff match {
              case Pos(x, _, _) if (x >= width || x < 0)             => Set()
              case Pos(_, y, _) if (y >= height || y < 0)            => Set()
              case p if (allAntennaCoordinates.contains((p.x, p.y))) => Set()
              case p                                                 => Set(p.copy(symbol = '#'))
            }
          }

          val subtracted: Set[Pos] = {
            pos - diff match {
              case Pos(x, y, _) if (x >= width || x < 0)             => Set()
              case Pos(x, y, _) if (y >= height || y < 0)            => Set()
              case p if (allAntennaCoordinates.contains((p.x, p.y))) => Set()
              case p                                                 => Set(p.copy(symbol = '#'))
            }
          }

          added ++ subtracted

        }

      }
      allAntinodesForAntenna
    }

    println("Antinodes")
    allAntinodes.flatten.toSet.foreach(println)

    allAntinodes.flatten.toSet.size

  }

}
