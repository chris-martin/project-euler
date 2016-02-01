package euler.problem

object Problem10 {

  lazy val answer: BigInt =
    Stream.range(2, 2000000)
      .map(BigInt(_))
      .filter(_.isProbablePrime(30))
      .sum

}
