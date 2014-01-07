object Problem65 {

  lazy val answer: Int = eContinuedFraction.convergent(100).toString.map(_.toString.toInt).sum

  val eContinuedFraction = ContinuedFraction(2, Iterable.fromIterator(
    Iterator.iterate(1)(_+1).flatMap(k => Seq(1, 2*k, 1))
  ))

  val twoContinuedFraction = ContinuedFraction(1, Seq(2).cycle)

  case class Fraction(numerator: Long, denominator: Long) {

    lazy val gcd: Long = BigInt(numerator).gcd(BigInt(denominator)).toLong

    def reduce: Fraction = Fraction(numerator / gcd, denominator / gcd)
    def inverse: Fraction = Fraction(denominator, numerator)

    def +(i: Int): Fraction = Fraction(numerator + i * denominator, denominator).reduce
    def *(i: Int): Fraction = Fraction(numerator * i, denominator)
  }

  implicit class FractionFriendlyInt(i: Int) {
    def +(f: Fraction): Fraction = f + i
    def /(f: Fraction): Fraction = f.inverse * i
  }

  case class ContinuedFraction(base: Int, repeat: Iterable[Int]) {

    def convergent(n: Int): Fraction =
      base + repeat.toStream.take(n-1).reverse.foldLeft(Fraction(0, 1)) {
        case (x, y) => 1 / ( y + x )
      }
  }

  implicit class RichIterableCompanion(I: Iterable.type) {

    def fromIterator[A](it: => Iterator[A]) = new Iterable[A] { override def iterator = it }
  }

  implicit class RichIterable[A](it: Iterable[A]) {

    def cycle: Iterable[A] = new Iterable[A] { override def iterator = Iterator cycle it }
  }

  implicit class RichIteratorCompantion(I: Iterator.type) {

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
