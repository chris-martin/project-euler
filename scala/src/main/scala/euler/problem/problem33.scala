package euler.problem

package object problem33 {

  implicit class IntString(s: String) {
    def digit(i: Int): Int = s(i).toString.toInt
  }

  final case class Fraction(c: Int, d: Int) {

    lazy val gcd = (BigInt(c) gcd BigInt(d)).toInt
    lazy val reduce: Fraction = reduceBy(gcd)
    def reduceBy(n: Int): Fraction = Fraction(c/gcd, d/gcd)

    def isCurious: Boolean =
      (
        for {
          a: String <- c.toString.permutations
          b: String <- d.toString.permutations
        }
          yield
            (a, b)

        ).exists { case (a, b) =>
          a.digit(0) != 0 && a(0) == b(0) && reduce == Fraction(a.digit(1), b.digit(1)).reduce
        }

    override def toString: String = "%d/%d".format(c, d)

  }

  implicit object FractionIsNumeric extends Numeric[Fraction] {

    def fromInt(x: Int) = Fraction(x, 1)
    def times(x: Fraction, y: Fraction) = Fraction(x.c*y.c, x.d*y.d)

    def plus(x: Fraction, y: Fraction) = ???
    def minus(x: Fraction, y: Fraction) = ???
    def negate(x: Fraction) = ???
    def toInt(x: Fraction) = ???
    def toLong(x: Fraction) = ???
    def toFloat(x: Fraction) = ???
    def toDouble(x: Fraction) = ???
    def compare(x: Fraction, y: Fraction) = ???
  }

  def specialFractions: Seq[Fraction] =
    (for (c <- 10 to 99; d <- c+1 to 99) yield Fraction(c, d))
      .filter(_.isCurious)

  def answer: Int =
    specialFractions.product.reduce.d

}
