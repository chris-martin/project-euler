package euler.problem

package object problem65 {

  def answer: Int = eContinuedFraction.convergent(100).numerator.sumOfDigits

  implicit class DigitSummingBigInt(i: BigInt) {
    def sumOfDigits: Int = i.toString.map({c:Char=>c.toString.toInt}).sum
  }

  val eContinuedFraction = ContinuedFraction(2, Iterable.fromIterator(
    Iterator.iterate(1)(_+1).flatMap(k => Seq(1, 2*k, 1))
  ))

  val twoContinuedFraction = ContinuedFraction(1, Seq(2).cycle)

  final case class Fraction(numerator: BigInt, denominator: BigInt) {

    lazy val gcd = numerator gcd denominator

    def reduce: Fraction = Fraction(numerator / gcd, denominator / gcd)
    def inverse: Fraction = Fraction(denominator, numerator)

    def +(i: Int): Fraction = Fraction(numerator + i * denominator, denominator).reduce
    def *(i: Int): Fraction = Fraction(numerator * i, denominator)
  }

  final case class ContinuedFraction(base: Int, repeat: Iterable[Int]) {

    def convergent(n: Int): Fraction =
      repeat.toStream.take(n-1).reverse.foldLeft(Fraction(0, 1)) {
        case (x, y) => ( x + y ).inverse
      } + base
  }

  implicit class RichIterableCompanion(I: Iterable.type) {

    def fromIterator[A](it: => Iterator[A]) = new Iterable[A] { override def iterator = it }
  }

  implicit class RichIterable[A](it: Iterable[A]) {

    def cycle: Iterable[A] = new Iterable[A] { override def iterator = Iterator cycle it }
  }

  implicit class RichIteratorCompanion(I: Iterator.type) {

    def cycle[A](iterable: Iterable[A]): Iterator[A] = new Iterator[A] {
      var iterator: Option[Iterator[A]] = None
      def hasNext = true
      def next() = {
        if (iterator.isEmpty) iterator = Some(iterable.iterator)
        val x = iterator.get.next()
        if (!iterator.get.hasNext) iterator = None
        x
      }
    }
  }
}
