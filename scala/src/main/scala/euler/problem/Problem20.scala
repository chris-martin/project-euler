package euler.problem

object Problem20 {

  lazy val answer: Int =
    (1 to 100)
      .map(BigInt(_))
      .product
      .toString
      .map(_.toString.toInt)
      .sum

}
