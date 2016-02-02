package euler.problem

import euler.util.prime.primes

object Problem50 {

  val primeSeq: Seq[Int] = primes.takeWhile(_ <= 999999).map(_.intValue).toSeq
  val primeSet: Set[Int] = primeSeq.toSet

  lazy val answer: Long =
    (0 until Stream.from(1).takeWhile(primeSeq.take(_).sum <= primeSeq.last).last)
      .reverseIterator
      .flatMap({ length =>
        (0 to primeSeq.length - length).map(a => primeSeq.slice(a, a + length).sum)
      })
      .filter(primeSet.contains).next()

}
