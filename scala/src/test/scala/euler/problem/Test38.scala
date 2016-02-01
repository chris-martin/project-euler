package euler.problem

import Problem38._

class Test38 extends org.scalatest.FreeSpec {

  "Example catProduct" in assertResult(192384576)(catProduct(192, 3))

  "Answer is correct" in assertResult(932718654)(answer)

}
