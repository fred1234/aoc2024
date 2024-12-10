object Day09 {

  sealed trait Block
  case class File(id: Int) extends Block:
    override def toString(): String = s"${id}"

  case class FreeSpace() extends Block:
    override def toString(): String = s"."

  type HardDisk = List[Block]

  def main(args: Array[String]): Unit = {
    val raw: String = os.read(os.pwd / "src" / "main" / "resources" / "day09.txt")
    // val raw = "2333133121414131402"
    println(part1(raw)) // 800232992 was too low
  }

  def part1(raw: String) = checkSum(defrag(generateBlocks(parse(raw))))

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

}
