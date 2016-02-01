package euler.problem

object Problem56 {

  lazy val answer: Int = (
    for (a <- 1 to 99; b <- 1 to 99)
    yield (BigInt(a) pow b).toString.map(_.toString.toInt).sum
  ).max

}
