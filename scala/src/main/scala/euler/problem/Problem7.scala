package euler.problem

object Problem7 {

  lazy val answer: Int =
    Stream.from(2).filter(_.isPrime)(10000)

  implicit class RichInt(i: Int) {

    def isPrime: Boolean =
      BigInt(i).isProbablePrime(30)

  }

}
