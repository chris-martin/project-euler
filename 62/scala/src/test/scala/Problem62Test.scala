class Problem62Test extends org.scalatest.FreeSpec {

  import Problem62._

  "Three permutations" in {
    assert ( permutationCubes(3) === 41063625 )
  }

  "Answer is correct" in {
    assert ( answer === BigInt("127035954683") )
  }

  "RichIterable.contiguouslyGroupBy" - {
    assert (
      Iterator("a1", "a2", "b1", "b2").contiguouslyGroupBy(_(0)).toSeq ===
        Seq(Seq("a1", "a2"), Seq("b1", "b2"))
    )
  }
}
