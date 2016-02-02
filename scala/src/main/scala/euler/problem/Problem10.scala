package euler.problem

import euler.util.prime.primes

object Problem10 {

  lazy val answer: BigInt = primes.takeWhile(_ <= 2000000).sum

}
