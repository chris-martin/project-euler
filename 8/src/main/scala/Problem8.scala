object Problem8 extends App {

  println(answer)

  def answer: Int =
    inputString
      .map(c => Integer.parseInt(c.toString))
      .sliding(5)
      .map(_.product)
      .max

  def inputString: String =
    io.Source.fromURL(
      getClass.getResource("number.txt")
    )
    .mkString.replaceAll("""\s""", "")

}
