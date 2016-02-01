package euler.problem

object Problem16 {

  lazy val answer: Int =
    BigInt(2).pow(1000).toString(10)
      .map(_.toString.toInt).sum

}
