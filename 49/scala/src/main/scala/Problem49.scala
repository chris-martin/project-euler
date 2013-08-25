object Problem49 {

  lazy val answer =
    (1000 to 9999).filter(BigInt(_).isProbablePrime(32))
    .groupBy(_.toString.sorted.mkString).values
    .flatMap(_.sorted.combinations(3))
    .filter(x => x(2) - x(1) == x(1) - x(0))
    .filterNot(_ == Seq(1487, 4817, 8147))
    .head.mkString

}
