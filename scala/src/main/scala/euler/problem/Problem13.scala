package euler.problem

object Problem13 {

  lazy val answer: String =
    io.Source.fromFile("../problems/13-data.txt")
      .getLines.map(_.trim).filter(_.nonEmpty)
      .map(BigInt(_)).sum.toString.substring(0, 10)

}
