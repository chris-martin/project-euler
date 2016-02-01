package euler.problem

object Problem22 {

  lazy val answer: Int =
    io.Source.fromFile("../problems/22-data.txt")
      .getLines.mkString.toUpperCase.split(",")
      .map(_.trim)
      .map(unquote)
      .sorted
      .map(name => name.map(_ - 'A' + 1).sum)
      .zip(Stream.from(1))
      .map({ case (x, y) => x * y })
      .sum

  val q = "\""

  def unquote(s: String): String =
    s.stripPrefix("\"").stripSuffix("\"")

}
