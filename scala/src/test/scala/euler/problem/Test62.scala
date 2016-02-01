package euler.problem

import Problem62._

class Test62 extends org.scalatest.FreeSpec {

  "Three permutations" in assertResult(41063625)(permutationCubes(3))

  "Answer is correct" in assertResult(BigInt("127035954683"))(answer)

  "RichIterable.contiguouslyGroupBy" in assert(
    Iterator("a1", "a2", "b1", "b2").contiguouslyGroupBy(_(0)).toSeq
    ===
    Seq(Seq("a1", "a2"), Seq("b1", "b2"))
  )

}
