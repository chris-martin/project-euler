package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem33._

class Test33 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(3000, Millis)

  "Fractions" in {
    info(specialFractions.mkString(", "))
    assertResult(4)(specialFractions.size)
  }

}
