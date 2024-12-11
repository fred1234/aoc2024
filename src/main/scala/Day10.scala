object Day10 {

  enum Dir:
    case UP, RIGHT, DOWN, LEFT

  case class Pos(x: Int, y: Int, height: Int):
    // override def toString(): String = s"$height"
    def move(dir: Dir, map: List[List[Pos]]): Option[Pos] = {
      dir match
        case Dir.UP    => map.lift(this.y - 1).flatMap(x => x.lift(this.x))
        case Dir.RIGHT => map.lift(this.y).flatMap(x => x.lift(this.x + 1))
        case Dir.DOWN  => map.lift(this.y + 1).flatMap(x => x.lift(this.x))
        case Dir.LEFT  => map.lift(this.y).flatMap(x => x.lift(this.x - 1))
    }

  def main(args: Array[String]): Unit = {

    val raw: List[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day10.txt").toList
    // val raw: List[String] = List( // has 9
    //   "89010123",
    //   "78121874",
    //   "87430965",
    //   "96549874",
    //   "45678903",
    //   "32019012",
    //   "01329801",
    //   "10456732"
    // )
    val hikingMap: List[List[Pos]] = parse(raw)

    val trailheads: List[Pos] = hikingMap.flatMap(line => line.filter(pos => pos.height == 0))
    // part1
    val scores: List[Set[Pos]] = trailheads.map(trailhead => countScores(trailhead, hikingMap))
    println(scores.map(_.size).sum)
    // part2
    // 235 is wrong
    val ratings = trailheads.flatMap(trailhead => countRatings(trailhead, hikingMap))
    println(ratings.sum)

  }

  def parse(raw: List[String]) = {
    raw.zipWithIndex.map { case (line, y) =>
      line.zipWithIndex.toList.map { case (height, x) =>
        Pos(x, y, height.toString().toInt)
      }
    }
  }

  def countScores(trailhead: Pos, hikingMap: List[List[Pos]]): Set[Pos] = {

    def getNeighbors(currentPos: Pos): Set[Pos] = {
      Dir.values.toList
        .map(dir => currentPos.move(dir, hikingMap))
        .flatten
        .filter { case Pos(_, _, height) => height == currentPos.height + 1 }
        .toSet
    }

    def score(pos: Pos): Set[Pos] = {
      if (pos.height == 9) {
        Set(pos)
      } else {
        getNeighbors(pos).flatMap(score)
      }
    }

    score(trailhead)
  }

  def countRatings(trailhead: Pos, hikingMap: List[List[Pos]]): List[Int] = {
    def getNeighbors(currentPos: Pos): Set[Pos] = {
      Dir.values.toList
        .map(dir => currentPos.move(dir, hikingMap))
        .flatten
        .filter { case Pos(_, _, height) => height == currentPos.height + 1 }
        .toSet
    }
    // def ratings(currentPos: Pos): Int = {
    //   if (currentPos.height == 9) {
    //     1
    //   } else {
    //     getNeighbors(currentPos).map(ratings).sum
    //   }
    // }

    def ratings(pos: Pos): List[Int] = {
      if (pos.height == 9) {
        List(1)
      } else {
        getNeighbors(pos).toList.flatMap(ratings)
      }
    }

    ratings(trailhead)
  }

}
