package euler.problem

package object problem63 {

  def answer: Int = powerfulNumbers.size

  // Searching for x of the form x = b^n where 10^(n-1) ≤ x < 10^n.
  lazy val powerfulNumbers: Set[BigInt] =
    (2 until 10).flatMap({ b => // [ x < 10^n ] ⇒ [ b^n < 10^n ] ⇒ [ b < 10 ]
      Iterator.from(1)
        .map({ n => (n, BigInt(b) pow n) })
        .takeWhile({ case (n, x) => x >= (BigInt(10) pow (n-1)) })
        .map({ case (n, x) => x })
    }).toSet + BigInt(1)

}
