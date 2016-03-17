package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem44._

class Test44 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(2000, Millis)

  val examplePentagonals = Seq(1, 5, 12, 22, 35, 51, 70, 92, 117, 145)

  "pentagonal" in assertResult(examplePentagonals) {
    (1 to examplePentagonals.size).map(pentagonal(_))
  }

  "isPentagonal" in {
    for (i <- 1 to 150)
      assertResult(examplePentagonals contains i)(isPentagonal(i))
  }

  "pentagonals" in assertResult(examplePentagonals) {
    pentagonals.take(examplePentagonals.size).map(_.p).toSeq
  }

}
