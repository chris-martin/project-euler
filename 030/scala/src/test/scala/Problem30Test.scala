class Problem30Test extends org.scalatest.FreeSpec {

  "Numbers" in {
    info(
      Problem30.magicNrs.mkString(", ")
    )
  }

  "Answer is correct" in {
    expectResult(443839) {
      Problem30.answer
    }
  }

}
