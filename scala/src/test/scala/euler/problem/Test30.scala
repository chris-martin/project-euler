package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Span, Millis}
import problem30._

class Test30 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(6000, Millis)

  "Numbers" in info(magicNrs.mkString(", "))

}
