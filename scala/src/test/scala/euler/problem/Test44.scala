package euler.problem

import Problem44._

class Test44 extends org.scalatest.FreeSpec {

  val examplePentagonals = Seq(1, 5, 12, 22, 35, 51, 70, 92, 117, 145)

  "pentagonal" in assertResult(examplePentagonals) {
    (1 to examplePentagonals.size).map(pentagonal(_))
  }

  "isPentagonal" in {
    for (i <- 1 to 150)
      assertResult(examplePentagonals contains i)(isPentagonal(i))
  }

  "pentagonals" in assertResult(examplePentagonals) {
      Problem44.pentagonals.take(examplePentagonals.size).map(_.p).toSeq
    }

  "Answer is correct" in assertResult(5482660)(answer)

}
