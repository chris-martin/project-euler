package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem65._

class Test65 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(200, Millis)

  "First ten convergents of sqrt(2)" in assert(
    (1 to 10).map(twoContinuedFraction.convergent)
    ===
    Seq(
      Fraction(1, 1),
      Fraction(3, 2),
      Fraction(7, 5),
      Fraction(17, 12),
      Fraction(41, 29),
      Fraction(99, 70),
      Fraction(239, 169),
      Fraction(577, 408),
      Fraction(1393, 985),
      Fraction(3363, 2378)
    )
  )

  "First ten convergents of sqrt(e)" in assert(
    (1 to 10).map(eContinuedFraction.convergent)
    ===
    Seq(
      Fraction(2, 1),
      Fraction(3, 1),
      Fraction(8, 3),
      Fraction(11, 4),
      Fraction(19, 7),
      Fraction(87, 32),
      Fraction(106, 39),
      Fraction(193, 71),
      Fraction(1264, 465),
      Fraction(1457, 536)
    )
  )

}
