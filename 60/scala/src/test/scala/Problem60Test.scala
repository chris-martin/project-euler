class Problem60Test extends org.scalatest.FreeSpec {

  import Problem60._

  "Size 4" in {
    assert ( smallestCliqueOfSize4 === Some(Set(3, 7, 109, 673)) )
  }

  "Size 5" in {
    assert ( smallestCliqueOfSize5 === Some(Set(13 + 5197 + 5701 + 6733 + 8389)) )
  }

  "Answer is correct" in {
    assert ( answer === 26033 )
  }
}
