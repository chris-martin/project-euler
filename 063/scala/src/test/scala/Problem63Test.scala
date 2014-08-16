class Problem63Test extends org.scalatest.FreeSpec {

  import Problem63._

  "Answer is correct" in {
    assert ( answer === 49 )
  }

  "7^5" in {
    powerfulNumbers contains (BigInt(7) pow 5)
  }

  "8^9" in {
    powerfulNumbers contains (BigInt(8) pow 9)
  }
}
