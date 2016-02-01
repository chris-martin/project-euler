package euler.problem

import Problem57._

class Test57 extends org.scalatest.FreeSpec {

  "fraction addition" in assert(
    Fraction(1, 2) + Fraction(2, 3) === Fraction(7, 6)
  )

  "Answer is correct" in assertResult(153)(answer)

}
