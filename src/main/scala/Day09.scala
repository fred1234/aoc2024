import scala.annotation.tailrec
object Day09 {

  sealed trait Block
  case class File(id: Int) extends Block:
    override def toString(): String = s"${id}"

  case class FreeSpace() extends Block:
    override def toString(): String = s"."

  type HardDisk = List[Block]

  def main(args: Array[String]): Unit = {
    val raw: String = os.read(os.pwd / "src" / "main" / "resources" / "day09.txt")
    println(part1(raw))
    println(part2(raw))

  }

  def part1(raw: String) = checkSum(defrag(generateBlocks(parse(raw))))
  def part2(raw: String) = checkSum(defragPart2(generateBlocks(parse(raw))))

  def parse(raw: String) = raw.map(_.toString.toInt).toList

  def generateBlocks(input: List[Int]): HardDisk = input.zipWithIndex.flatMap((size, index) =>
    List.fill(size)(if index % 2 == 0 then File(index / 2) else FreeSpace())
  )

  def defrag(hd: HardDisk): HardDisk = {
    def swapTwoBlocks(hd: HardDisk, i1: Int, i2: Int) = hd.updated(i1, hd(i2)).updated(i2, hd(i1))
    def defrag(hd: HardDisk, lIdx: Int, rIdx: Int): HardDisk = {
      (hd(lIdx), hd(rIdx)) match
        case _ if lIdx >= rIdx          => hd
        case (FreeSpace(), FreeSpace()) => defrag(hd, lIdx, rIdx - 1)
        case (FreeSpace(), File(id))    => defrag(swapTwoBlocks(hd, lIdx, rIdx), lIdx + 1, rIdx - 1)
        case (File(id), FreeSpace())    => defrag(hd, lIdx + 1, rIdx)
        case (File(id), File(idRight))  => defrag(hd, lIdx + 1, rIdx)
    }
    defrag(hd, lIdx = 0, rIdx = hd.size - 1)
  }

  def checkSum(hd: HardDisk): Long = {
    hd.zipWithIndex.map {
      case (File(id), index) => id * index.toLong
      case _                 => 0
    }.sum
  }

  def defragPart2(hd: HardDisk): HardDisk = {
    // huge thanks to https://github.com/fdlk/advent-2024 ❤️
    def defrag(hd: HardDisk, id: Int): HardDisk = {

      if id < 0 then hd
      else {

        val fileStart = hd.indexOf(File(id))
        val fileEnd = hd.lastIndexOf(File(id))

        val indices: Range = hd.indices
        val filtered: List[Int] = indices.filter(_ < fileStart).toList
        val potFreeSlots: List[List[Int]] =
          filtered.map(start => (fileStart to fileEnd).indices.toList.map(_ + start))

        val foundSlot: Option[List[Int]] =
          potFreeSlots.find(potFreeSlot => potFreeSlot.forall(block => hd(block) == FreeSpace()))

        val defragmentedHD: Option[List[Block]] = foundSlot
          .map(freeSlots =>
            hd.zipWithIndex.map({
              case (File(blockId), _) if blockId == id => FreeSpace() // replace all Files at the end with free space
              case (_, index) if freeSlots.contains(index) => File(id) // replace the free slots with the file
              case (block, _)                              => block
            })
          )
        println(s"going to id: ${id - 1}")
        defrag(defragmentedHD.getOrElse(hd), id - 1)
      }
    }
    val maxID = hd.map {
      case File(id)    => id
      case FreeSpace() => 0
    }.max

    defrag(hd, maxID)

  }

}
