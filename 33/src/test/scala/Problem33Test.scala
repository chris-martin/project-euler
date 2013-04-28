class Problem33Test extends org.scalatest.FreeSpec {

  "Number of results is 4" in {
    expectResult(4) {
      Problem33.specialFractions.size
    }
  }

  "Answer is correct" in {
    expectResult(100) {
      Problem33.answer
    }
  }

}
