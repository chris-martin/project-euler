class Problem44Test extends org.scalatest.FreeSpec {

  val examplePentagonals = Seq(1, 5, 12, 22, 35, 51, 70, 92, 117, 145)

  "pentagonal" in {
    expectResult(examplePentagonals) {
      (1 to examplePentagonals.size).map(Problem44.pentagonal(_))
    }
  }

  "isPentagonal" in {
    for (i <- 1 to 150) {
      expectResult(examplePentagonals contains i) {
        Problem44.isPentagonal(i)
      }
    }
  }

  "pentagonals" in {
    expectResult(examplePentagonals) {
      Problem44.pentagonals.take(examplePentagonals.size).map(_.p).toSeq
    }
  }

  "Answer is correct" in {
    expectResult(5482660) {
      Problem44.answer
    }
  }

}
