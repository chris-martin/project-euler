package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem32._

class Test32 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(10000, Millis)

  // todo - This is too slow to run with the general test population
  // "Products" in info(products.mkString(", "))

}
