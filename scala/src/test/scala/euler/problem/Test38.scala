package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem38._

class Test38 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(2000, Millis)

  "Example catProduct" in assertResult(192384576)(catProduct(192, 3))

}
