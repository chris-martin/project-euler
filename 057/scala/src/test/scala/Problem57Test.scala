class Problem57Test extends org.scalatest.FreeSpec {

  import Problem57.Fraction

  "fraction addition" in {
    assert (
      Fraction(1, 2) + Fraction(2, 3)
      ===
      Fraction(7, 6)
    )
  }

  "Answer is correct" in {
    expectResult(153) {
      Problem57.answer
    }
  }

}
