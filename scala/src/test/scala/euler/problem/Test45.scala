package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem45._

class Test45 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(2000, Millis)

  "isTriangle" in {
    for (z <- Seq(1, 3, 6, 10, 40755)) assert(isTriangle(z))
  }

  "isPentagonal" in {
    for (z <- Seq(1, 5, 12, 22, 35, 40755)) assert(isPentagonal(z))
  }

  "isHexagonal" in {
    for (z <- Seq(1, 6, 15, 28, 45, 40755)) assert(isHexagonal(z))
  }

}
