class Problem66Test extends org.scalatest.FreeSpec {

  import Problem66._

  "solve for y" - {
    "3^2 – 2 × 2^2 = 1" in { assert ( solveForY(3, 2) === Some(2) ) }
    "2^2 – 3 × 1^2 = 1" in { assert ( solveForY(2, 3) === Some(1) ) }
    "9^2 – 5 × 4^2 = 1" in { assert ( solveForY(9, 5) === Some(4) ) }
    "5^2 – 6 × 2^2 = 1" in { assert ( solveForY(5, 6) === Some(2) ) }
    "8^2 – 7 × 3^2 = 1" in { assert ( solveForY(8, 7) === Some(3) ) }
  }

  "≤ 7" in {
    assert ( dMaximizingX(7) === 5 )
  }

  "Answer is correct" in {
    assert ( answer === 0 ) // not 449
  }
}
