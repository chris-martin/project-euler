package euler.problem

object Problem42 {

  lazy val triangles =
    Stream.from(1).map(n => n * (n + 1) / 2)

  lazy val answer: Int =
    io.Source.fromFile("../problems/42-data.txt")
      .mkString
      .replace("\"", "")
      .split(",")
      .map(_.toUpperCase.map(_ - 'A' + 1).sum)
      .count(v => triangles.takeWhile(_ <= v).contains(v))

}
