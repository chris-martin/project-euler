package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem39._

class Test39 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(200, Millis)

  val q = 61274L

  "root of a square"     in assertResult(Some(q))(sqrt(q*q))

  "root of a non-square" in assertResult(None)(sqrt(q*q + 1))

}
