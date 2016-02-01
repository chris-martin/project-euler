package euler.problem

import Problem32._

class Test32 extends org.scalatest.FreeSpec {

  "Products" in info(products.mkString(", "))

  "Answer is correct" in assertResult(45228)(answer)

}
