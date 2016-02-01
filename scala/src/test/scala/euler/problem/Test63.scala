package euler.problem

import Problem63._

class Test63 extends org.scalatest.FreeSpec {

  "Answer is correct" in assertResult(49)(answer)

  "7^5" in assert(powerfulNumbers contains (BigInt(7) pow 5))

  "8^9" in assert(powerfulNumbers contains (BigInt(8) pow 9))

}
