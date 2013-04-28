object Problem7 extends App {

  println(answer)

  def answer: Int =
    Stream.from(2).filter(_.isPrime)(10000)

  implicit class RichInt(i: Int) {

    def isPrime: Boolean = BigInt(i).isProbablePrime(30)

  }

}
