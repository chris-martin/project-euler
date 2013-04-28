object Problem13 extends App {

  println(answer)

  def answer: String =
    io.Source.fromURL(getClass.getResource("numbers.txt"))
      .getLines.map(_.trim).filter(_.nonEmpty)
      .map(BigInt(_)).sum.toString.substring(0, 10)

}
