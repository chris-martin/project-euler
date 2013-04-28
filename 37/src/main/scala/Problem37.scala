object Problem37 extends App {

  println("Primes: %s".format(interestingPrimes.mkString(", ")))

  println("Sum: %s".format(answer))

  def truncations(s: String): Seq[String] =
    Seq(s) ++
      (1 to s.length-1).map(s.toSeq.drop(_).mkString) ++
      (1 to s.length-1).map(s.toSeq.take(_).mkString)

  lazy val interestingPrimes = Stream.from(11).map(BigInt(_)).filter({ i =>
    truncations(i.toString()).forall(BigInt(_).isProbablePrime(40))
  }).take(11)

  def answer = interestingPrimes.sum

}
