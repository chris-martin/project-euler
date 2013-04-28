class Test extends org.scalatest.FreeSpec {

  "Answer is correct" in {
    expectResult(233168) {
      Main.answer
    }
  }

}
