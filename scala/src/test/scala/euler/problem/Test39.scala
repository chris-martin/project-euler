package euler.problem

import Problem39._

class Test39 extends org.scalatest.FreeSpec {

  val q = 61274L

  "root of a square"     in assertResult(Some(q))(sqrt(q*q))

  "root of a non-square" in assertResult(None)(sqrt(q*q + 1))

  "Answer is correct"    in assertResult(840)(answer)

}
