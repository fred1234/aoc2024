import os.makeDir.all
import Day07.add
import scala.collection.MapView
object Day08 {

  case class Pos(x: Int, y: Int, symbol: String):
    def -(that: Pos): Pos = copy(x = x - that.x, y = y - that.y)
    def minusFactor(that: Pos, factor: Int): Pos = copy(x = x - that.x * factor, y = y - that.y * factor)
    def +(that: Pos): Pos = copy(x = x + that.x, y = y + that.y)
    def plusFactor(that: Pos, factor: Int): Pos = copy(x = x + that.x * factor, y = y + that.y * factor)

  def main(args: Array[String]): Unit = {
    val raw: List[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day08.txt").toList

    val height = raw.size
    val width = raw(0).size

    val world: List[Pos] = parse(raw)

    println(solvePart1(world, height, width))
    println(solvePart2(world, height, width))

  }

  def parse(raw: List[String]): List[Pos] = {
    for {
      y <- raw.indices
      x <- raw(0).indices
    } yield {
      Pos(x, y, raw(y).charAt(x).toString())
    }
  }.toList

  def solvePart1(world: List[Pos], height: Int, width: Int) = {

    val allAntennas: List[Pos] = world.filter(_.symbol != ".")
    val groupedByAntenna: Map[String, Set[Pos]] = allAntennas.groupBy(_.symbol).mapValues(_.toSet).toMap

    val allAntinodes: Set[Pos] = groupedByAntenna.toSet.flatMap { (antenna, listOfPos) =>

      // for a give antenna-type, here we have all the differences of all combinations
      val allDiffs: List[(Pos, Pos, Pos)] = listOfPos.toList
        .combinations(2)
        .map {
          case List(first, second) => (second - first, first, second)
          case _                   => ???
        }
        .toList

      // now we collect the possible locations.
      val allAntinodesForAntenna: Set[Pos] = listOfPos.flatMap { pos =>
        allDiffs.flatMap { case (diff, antennaA, antennaB) =>
          val subtracted: Set[Pos] = {
            antennaA - diff match {
              case Pos(x, _, _) if (x >= width || x < 0)  => Set()
              case Pos(_, y, _) if (y >= height || y < 0) => Set()
              case p                                      => Set(p.copy(symbol = "#"))
            }
          }

          val added: Set[Pos] = {
            antennaB + diff match {
              case Pos(x, y, _) if (x >= width || x < 0)  => Set()
              case Pos(x, y, _) if (y >= height || y < 0) => Set()
              case p                                      => Set(p.copy(symbol = "#"))
            }
          }

          added ++ subtracted

        }

      }
      allAntinodesForAntenna
    }

    allAntinodes.size

  }

  def solvePart2(world: List[Pos], height: Int, width: Int) = {

    val allAntennas: List[Pos] = world.filter(_.symbol != ".")
    val groupedByAntenna: Map[String, Set[Pos]] = allAntennas.groupBy(_.symbol).mapValues(_.toSet).toMap

    val allAntinodes = groupedByAntenna.toSet.flatMap { (antenna, listOfPos) =>

      // for a give antenna-type, here we have all the differences of all combinations
      val allDiffs: List[(Pos, Pos, Pos)] = listOfPos.toList
        .combinations(2)
        .map {
          case List(first, second) => (second - first, first, second)
          case _                   => ???
        }
        .toList

      // now we collect the possible locations.
      val allAntinodesForAntenna: Set[Pos] = listOfPos.flatMap { pos =>
        allDiffs.flatMap { case (diff, antennaA, antennaB) =>
          val subtracted = (1 to width).flatMap { case factor =>
            antennaA.plusFactor(diff, factor) match {
              case Pos(x, _, _) if (x >= width || x < 0)  => Set()
              case Pos(_, y, _) if (y >= height || y < 0) => Set()
              case p                                      => Set(p.copy(symbol = s"#"))
            }
          }.toSet

          val added = (1 to width).flatMap { case factor =>
            antennaB.minusFactor(diff, factor) match {
              case Pos(x, _, _) if (x >= width || x < 0)  => Set()
              case Pos(_, y, _) if (y >= height || y < 0) => Set()
              case p                                      => Set(p.copy(symbol = s"#"))
            }
          }.toSet

          added ++ subtracted
        }

      }
      allAntinodesForAntenna
    }

    allAntinodes.size

  }

}
