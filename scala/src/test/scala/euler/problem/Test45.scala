package euler.problem

import Problem45._

class Test45 extends org.scalatest.FreeSpec {

  "isTriangle" in {
    for (z <- Seq(1, 3, 6, 10, 40755)) assert(isTriangle(z))
  }

  "isPentagonal" in {
    for (z <- Seq(1, 5, 12, 22, 35, 40755)) assert(isPentagonal(z))
  }

  "isHexagonal" in {
    for (z <- Seq(1, 6, 15, 28, 45, 40755)) assert(isHexagonal(z))
  }

  "Answer is correct" in assertResult(1533776805)(answer)

}
