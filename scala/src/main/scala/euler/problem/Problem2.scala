package euler.problem

import euler.util.fibonacci.fibs

object Problem2 {

  lazy val answer: Int =
    fibs
    .takeWhile (_ < 4000000)
    .filter (_ % 2 == 0)
    .sum
    .toInt

}
