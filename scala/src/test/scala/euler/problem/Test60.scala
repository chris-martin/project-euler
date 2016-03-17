package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem60._

class Test60 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(5000, Millis)

  "Size 4" in assert(
    smallestCliqueOfSize4 === Some(Set(3, 7, 109, 673))
  )

// todo - This is too slow to run with the general test population
//  "Size 5" in assert(
//    smallestCliqueOfSize5 === Some(Set(13, 5197, 5701, 6733, 8389))
//  )

}
