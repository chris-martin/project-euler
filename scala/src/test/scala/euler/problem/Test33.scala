package euler.problem

import Problem33._

class Test33 extends org.scalatest.FreeSpec {

  "Fractions" in {
    info(specialFractions.mkString(", "))
    assertResult(4)(specialFractions.size)
  }

  "Answer is correct" in assertResult(100)(answer)

}
