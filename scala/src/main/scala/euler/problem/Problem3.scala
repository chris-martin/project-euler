package euler.problem

import euler.util.prime.primes

object Problem3 {

  lazy val answer: Int = {

    var x = BigInt("600851475143")
    while (!x.isProbablePrime(40)) {
      x /= primes.filter(i => (x mod i) == 0).head
    }

    x.toInt
  }

}
