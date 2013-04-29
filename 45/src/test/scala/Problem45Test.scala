class Problem45Test extends org.scalatest.FreeSpec {

  "isTriangle" in {
    for (z <- Seq(1, 3, 6, 10, 40755)) {
      assert(Problem45.isTriangle(z))
    }
  }

  "isPentagonal" in {
    for (z <- Seq(1, 5, 12, 22, 35, 40755)) {
      assert(Problem45.isPentagonal(z))
    }
  }

  "isHexagonal" in {
    for (z <- Seq(1, 6, 15, 28, 45, 40755)) {
      assert(Problem45.isHexagonal(z))
    }
  }

  "Answer is correct" in {
    expectResult(1533776805) {
      Problem45.answer
    }
  }

}
