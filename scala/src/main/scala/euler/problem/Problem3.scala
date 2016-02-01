package euler.problem

object Problem3 {

  lazy val answer: Int = {

    val primes = Stream.from(0).map(BigInt(_))
      .filter(_.isProbablePrime(10)).map(_.toInt)

    var x = BigInt("600851475143")
    while (!x.isProbablePrime(40)) {
      x /= primes.filter(i => (x mod i) == 0).head
    }

    x.toInt
  }

}
