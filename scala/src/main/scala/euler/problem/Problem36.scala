package euler.problem

import euler.util.palindrome.palindromeInt

object Problem36 {

  lazy val answer: Int =
    (1 to 999999)
      .map(BigInt(_))
      .filter(palindromeInt(_, base=10))
      .filter(palindromeInt(_, base=2 ))
      .sum
      .toInt

}
