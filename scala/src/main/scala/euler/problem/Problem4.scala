package euler.problem

import euler.util.palindrome.palindromeInt

object Problem4 extends App {

  lazy val answer: Int =
    (1 to 999)
      .map(BigInt(_))
      .combinations(2)
      .map(x => x(0) * x(1))
      .filter(palindromeInt(_))
      .max
      .intValue

}
