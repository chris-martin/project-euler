object Problem8 {

  lazy val answer: Int =
    io.Source.fromURL(getClass.getResource("number.txt"))
      .getLines
      .map(_.trim)
      .flatMap(_.map(_.toString.toInt))
      .sliding(5)
      .map(_.product)
      .max

}
