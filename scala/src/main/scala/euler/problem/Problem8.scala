package euler.problem

object Problem8 {

  lazy val answer: Int =
    io.Source.fromFile("../problems/8-data.txt")
      .getLines
      .map(_.trim)
      .flatMap(_.map(_.toString.toInt))
      .sliding(5)
      .map(_.product)
      .max

}
