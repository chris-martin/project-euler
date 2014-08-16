object Problem22 {

  lazy val answer: Int =
    io.Source.fromURL(getClass.getResource("names.txt"))
      .getLines.mkString.toUpperCase.split(",")
      .map(_.trim.stripPrefix("\"").stripSuffix("\""))
      .sorted
      .map(name => name.map(_ - 'A' + 1).sum)
      .zip(Stream.from(1))
      .map(x => x._1 * x._2)
      .sum

}
