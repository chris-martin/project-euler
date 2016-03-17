package euler.problem

package object problem27 extends {

  def answer: Int = {

    val expressions =
      for {
        a <- -999 to 999
        b <- -999 to 999
      }
      yield
        new QuadExpression(a, b)

    val best = expressions.maxBy(_.nrOfPrimes)

    best.a * best.b
  }

  final case class QuadExpression(a: Int, b: Int) {

    def apply(n: Int): Int = n*n + a*n + b

    lazy val nrOfPrimes: Int =
      Stream.from(0)
        .takeWhile(n => BigInt(apply(n)).isProbablePrime(40))
        .size

  }

}
