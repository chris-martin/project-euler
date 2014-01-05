class Problem60Test extends org.scalatest.FreeSpec {

  import Problem60._

  "Size 4" in {
    assert ( smallestCliqueOfSize4 === Some(Set(3, 7, 109, 673)) )
  }

  "Size 5" in {
    assert ( smallestCliqueOfSize5 === Some(Set(5381, 5507, 7877, 41621, 47237)) )
  }

  "Answer is correct" in {
    assert ( answer === 107623 )
  }
}
