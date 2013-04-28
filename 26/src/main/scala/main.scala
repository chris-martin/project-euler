object Problem26 extends App {

  println(answer)

  def answer: Int =
    (2 to 999).map(Fraction(1, _)).maxBy(_.period).d.toInt

  case class Fraction(c: BigInt, d: BigInt) {

    lazy val period: Int = {

      /*
        This algorithm only works for c=1.
        It relies on the fact that the period of 1/d is less than d
       */
      assert(c == 1)

      assert(d.isValidInt)

      (
        Stream.from(0) flatMap { i =>
          (1 until d.toInt) find { period =>
            shiftLeft(i).remainder.reduce == shiftLeft(i+period).remainder.reduce
          }
        }
      ).head

    }

    def remainder: Fraction = Fraction(c mod d, d)
    def shiftLeft(n: Int): Fraction = *(BigInt(10) pow n)
    def *(x: BigInt): Fraction = Fraction(c*x, d)
    def reduce: Fraction = { val x = c gcd d; Fraction(c/x, d/x) }
    override def toString: String = "%s/%s".format(c, d)

  }

}
