package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem57._

class Test57 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(200, Millis)

  "fraction addition" in assert(
    Fraction(1, 2) + Fraction(2, 3) === Fraction(7, 6)
  )

}
