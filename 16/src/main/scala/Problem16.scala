object Problem16 extends App {

  println(answer)

  def answer: Int =
    BigInt(2).pow(1000).toString(10)
      .map(_.toString.toInt).sum

}
