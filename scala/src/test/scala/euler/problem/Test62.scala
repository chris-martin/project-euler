package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem62._

class Test62 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(200, Millis)

  "Three permutations" in assertResult(41063625)(permutationCubes(3))

  "RichIterable.contiguouslyGroupBy" in assert(
    Iterator("a1", "a2", "b1", "b2").contiguouslyGroupBy(_(0)).toSeq
    ===
    Seq(Seq("a1", "a2"), Seq("b1", "b2"))
  )

}
