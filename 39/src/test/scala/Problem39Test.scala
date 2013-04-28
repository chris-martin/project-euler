class Problem39Test extends org.scalatest.FreeSpec {

  val q = 61274L

  "root of a square" in {
    expectResult(Some(q)) {
      Problem39.sqrt(q*q)
    }
  }

  "root of a non-square" in {
    expectResult(None) {
      Problem39.sqrt(q*q + 1)
    }
  }

  "Answer is correct" in {
    expectResult(840) {
      Problem39.answer
    }
  }

}
