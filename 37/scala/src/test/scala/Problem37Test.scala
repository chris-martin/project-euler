class Problem37Test extends org.scalatest.FreeSpec {

  "Primes" in {
    info(
      Problem37.interestingPrimes.mkString(", ")
    )
  }

  "Answer is correct" in {
    expectResult(748317) {
      Problem37.answer
    }
  }

}
