object Problem37 {

  def truncations(s: String): Seq[String] =
    Seq(s) ++
      (1 to s.length-1).map(s.toSeq.drop(_).mkString) ++
      (1 to s.length-1).map(s.toSeq.take(_).mkString)

  lazy val interestingPrimes = Stream.from(11).map(BigInt(_)).filter({ i =>
    truncations(i.toString()).forall(BigInt(_).isProbablePrime(40))
  }).take(11)

  lazy val answer = interestingPrimes.sum

}
