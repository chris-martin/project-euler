package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem37._

class Test37 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(20000, Millis)

  "Primes" in info(interestingPrimes.mkString(", "))

}
