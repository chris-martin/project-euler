class Problem66Test extends org.scalatest.FreeSpec {

  import Problem66._

  "solve for x" - {
    "3^2 – 2 × 2^2 = 1" in { assert ( solveForX(y=2, d=2) === Some(3) ) }
    "2^2 – 3 × 1^2 = 1" in { assert ( solveForX(y=1, d=3) === Some(2) ) }
    "9^2 – 5 × 4^2 = 1" in { assert ( solveForX(y=4, d=5) === Some(9) ) }
    "5^2 – 6 × 2^2 = 1" in { assert ( solveForX(y=2, d=6) === Some(5) ) }
    "8^2 – 7 × 3^2 = 1" in { assert ( solveForX(y=3, d=7) === Some(8) ) }
  }

  "≤ 7" in {
    assert ( dMaxMin(7) === 5 )
  }

  "Answer is correct" in {
    assert ( answer === 0 )
  }
}
