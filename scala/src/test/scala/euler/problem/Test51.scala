package euler.problem

import Problem51._

class Test51 extends org.scalatest.FreeSpec {

  "RichIterable.groupByMultiple" in assert(
    (18 to 22).toSet.groupByMultiple(_.toString.toSeq)
    ===
    Map(
      '0' -> Set(20),
      '1' -> Set(18, 19, 21),
      '2' -> Set(20, 21, 22),
      '8' -> Set(18),
      '9' -> Set(19)
    )
  )

  "RichSeq.replacementPossibilities" in assert(
    "a -- b".toSeq.replacementPossibilities('-', 'X').map(_.mkString)
    ===
      Seq("a -- b", "a X- b", "a -X b", "a XX b")
  )

  "RichString.starPossibilities" in assert(
    "1233".starPossibilities
    ===
    Seq("*233", "1*33", "12*3", "123*", "12**")
  )

  "smallestPrimeValueFamily(7)" in assert(
    Problem51.smallestPrimeValueFamily(7).members.toSeq
    ===
    Seq(56003, 56113, 56333, 56443, 56663, 56773, 56993)
  )

  "Answer is correct" in assertResult(121313)(answer)

}
