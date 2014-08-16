class Problem64Test extends org.scalatest.FreeSpec {

  import Problem64._

  "Answer is correct" in {
    assert ( answer === 1322 )
  }

  "N ≤ 13" in {
    assert ( countOddPeriods(13) === 4 )
  }

  "sqrt(2)" in { assert ( continuedFraction(2) === ContinuedFraction(1, Seq(2)) ) }
  "sqrt(3)" in { assert ( continuedFraction(3) === ContinuedFraction(1, Seq(1, 2)) ) }
  "sqrt(4)" in { assert ( continuedFraction(4) === ContinuedFraction(2, Seq()) ) }
  "sqrt(5)" in { assert ( continuedFraction(5) === ContinuedFraction(2, Seq(4)) ) }
  "sqrt(6)" in { assert ( continuedFraction(6) === ContinuedFraction(2, Seq(2, 4)) ) }
  "sqrt(7)" in { assert ( continuedFraction(7) === ContinuedFraction(2, Seq(1, 1, 1, 4)) ) }
  "sqrt(8)" in { assert ( continuedFraction(8) === ContinuedFraction(2, Seq(1, 4)) ) }
  "sqrt(9)" in { assert ( continuedFraction(9) === ContinuedFraction(3, Seq()) ) }
  "sqrt(10)" in { assert ( continuedFraction(10) === ContinuedFraction(3, Seq(6)) ) }
  "sqrt(11)" in { assert ( continuedFraction(11) === ContinuedFraction(3, Seq(3, 6)) ) }
  "sqrt(12)" in { assert ( continuedFraction(12) === ContinuedFraction(3, Seq(2, 6)) ) }
  "sqrt(13)" in { assert ( continuedFraction(13) === ContinuedFraction(3, Seq(1, 1, 1, 1, 6)) ) }
}
