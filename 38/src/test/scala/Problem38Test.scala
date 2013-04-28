class Problem38Test extends org.scalatest.FreeSpec {

  "Example catProduct" in {
    expectResult(192384576) {
      Problem38.catProduct(192, 3)
    }
  }

  "Answer is correct" in {
    expectResult(932718654) {
      Problem38.answer
    }
  }

}
