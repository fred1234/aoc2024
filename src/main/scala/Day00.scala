object Day00 {
  def main(args: Array[String]): Unit = {
    println("""
   █████╗  ██████╗  ██████╗
  ██╔══██╗██╔═══██╗██╔════╝
  ███████║██║   ██║██║     
  ██╔══██║██║   ██║██║     
  ██║  ██║╚██████╔╝╚██████╗
  ╚═╝  ╚═╝ ╚═════╝  ╚═════╝""")

    val raw: List[String] = os.read.lines(os.pwd / "src" / "main" / "resources" / "day00.txt").toList

    val parsed = raw.map(_.toUpperCase)
    parsed.foreach(println)

  }
}
