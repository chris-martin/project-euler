class Problem33Test extends org.scalatest.FreeSpec {

  "Fractions" in {
    info(
      Problem33.specialFractions.mkString(", ")
    )
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
