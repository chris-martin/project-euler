object Main {
  def main(args: Array[String]) {

    val expressions =
      for {
        a <- (-999 to 999)
        b <- (-999 to 999)
      }
      yield
        new QuadExpression(a, b)

    val best = expressions.maxBy(_.nrOfPrimes)

    println(best.a * best.b)

  }
}

case class QuadExpression(a: Int, b: Int) {

  def apply(n: Int): Int = n*n + a*n + b

  lazy val nrOfPrimes: Int = Stream.from(0).takeWhile(n => BigInt(apply(n)).isProbablePrime(40)).size

}
