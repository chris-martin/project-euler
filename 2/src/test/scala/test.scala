class Test extends org.scalatest.FreeSpec {

  "Answer is correct" in {
    expectResult(4613732) {
      Main.answer
    }
  }

}
