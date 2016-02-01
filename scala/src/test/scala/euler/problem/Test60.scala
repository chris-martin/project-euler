package euler.problem

import Problem60._

class Test60 extends org.scalatest.FreeSpec {

  "Size 4" in assert(
    smallestCliqueOfSize4 === Some(Set(3, 7, 109, 673))
  )

  "Size 5" in assert(
    smallestCliqueOfSize5 === Some(Set(13, 5197, 5701, 6733, 8389))
  )

  "Answer is correct" in assertResult(26033)(answer)

}
