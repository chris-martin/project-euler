package euler.problem

import Problem37._

class Test37 extends org.scalatest.FreeSpec {

  "Primes" in info(interestingPrimes.mkString(", "))

  "Answer is correct" in assertResult(748317)(answer)

}
