package euler.problem

package object problem37 {

  def truncations(s: String): Seq[String] =
    Seq(s) ++
      (1 until s.length).map(s.toSeq.drop(_).mkString) ++
      (1 until s.length).map(s.toSeq.take(_).mkString)

  def interestingPrimes = Stream.from(11).map(BigInt(_)).filter({ i =>
    truncations(i.toString()).forall(BigInt(_).isProbablePrime(40))
  }).take(11)

  def answer = interestingPrimes.sum

}
