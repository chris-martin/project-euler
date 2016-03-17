package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem63._

class Test63 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(200, Millis)

  "7^5" in assert(powerfulNumbers contains (BigInt(7) pow 5))

  "8^9" in assert(powerfulNumbers contains (BigInt(8) pow 9))

}
