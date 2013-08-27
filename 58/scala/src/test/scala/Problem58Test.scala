class Problem58Test extends org.scalatest.FreeSpec {

  import Problem58._

  "Answer is correct" in {
    assert ( answer === 26241 )
  }

  "Rings" in {
    assert (
      rings.take(4).toList
      ===
      Ring(0, 1::Nil) ::
      Ring(1, 3::5::7::9::Nil)::
      Ring(2, 13::17::21::25::Nil)::
      Ring(3, 31::37::43::49::Nil)::
      Nil
    )
  }

  "Fraction concatenation" in {
    assert ( Fraction(0, 1) ++ Fraction(3, 4) === Fraction(3, 5) )
  }

  "Fraction 0" in {
    assert ( rings(0).primeFraction === Fraction(0, 1) )
  }

  "Fraction 1" in {
    assert ( rings(1).primeFraction === Fraction(3, 4) )
  }

  "Fraction 2" in {
    assert ( rings(2).primeFraction === Fraction(2, 4) )
  }

  "Running fraction 1" in {
    assert ( runningFraction(1) === (rings(1), Fraction(3, 5)) )
  }

  "Running fraction 3" in {
    assert ( runningFraction(3) === (rings(3), Fraction(8, 13)) )
  }

}
