class Problem44Test extends org.scalatest.FreeSpec {

  val examplePentagonals = Seq(1, 5, 12, 22, 35, 51, 70, 92, 117, 145)

  "pentagonal works on the examples" in {
    expectResult(examplePentagonals) {
      (1 to examplePentagonals.size).map(Problem44.pentagonal(_))
    }
  }

  "isPentagonal works on the examples" in {
    for (i <- 1 to 150) {
      expectResult(examplePentagonals contains i) {
        Problem44.isPentagonal(i)
      }
    }
  }

  "Answer is correct" in {
    info(Problem44.answer.toString)
  }

}
