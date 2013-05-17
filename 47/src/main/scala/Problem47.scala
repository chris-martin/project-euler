object Problem47 {

  type Prime = Stream[Int]

  val primes: Stream[Prime] = Stream.iterate
    { Stream.from(2).filter(BigInt(_).isProbablePrime(40)) }
    { _.drop(1) }

  type PrimeSet = Stream[List[Prime]]

  val firstPrimeSet: PrimeSet = Stream(Nil)





  /** @param factors list of (factor -> multiplicity)
    *                with factors in descending order
    */
  case class Factorization(factors: List[(Prime, Int)])

  val factorizations = new collection.mutable.ArrayBuffer[Option[Factorization]] {
    override def apply(idx: Int) = if (idx >= length) None else super.apply(idx)
  }

  factorizations.update(1, Some(Factorization(Nil)))

  var highestFactorizedValue = 1

  lazy val answer: Int = {
    5
  }

}
