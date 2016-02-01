package euler.problem

import Problem30._

class Test30 extends org.scalatest.FreeSpec {

  "Numbers" in info(magicNrs.mkString(", "))

  "Answer is correct" in assertResult(443839)(answer)

}
