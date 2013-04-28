object Problem20 extends App {

  println(answer)

  def answer: Int =
    (1 to 100).map(BigInt(_)).product
      .toString.map(_.toString.toInt).sum

}
