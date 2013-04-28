object Problem44 {

  case class Polynomial(coeffs: List[Long]) {

    // http://rosettacode.org/wiki/Horner's_rule_for_polynomial_evaluation
    def apply(x: Long): Long =
      coeffs.reverse.foldLeft(0L){ (a, c) => a * x + c }

    final def rootBetween(x1: Long, x2: Long): Option[Long] =
      rootBetween(x1, apply(x1), x2, apply(x2))

    @annotation.tailrec
    private final def rootBetween(x1: Long, y1: Long, x2: Long, y2: Long): Option[Long] = {

      if (y1.signum == 0)
        Some(x1)
      else if (y2.signum == 0)
        Some(x2)
      else if (y1.signum == y2.signum)
        None
      else if ((x1-x2).abs <= 1)
        None
      else {
        val x = math.min(x1, x2) + (x1-x2).abs / 2
        val y = apply(x)
        if (y.signum == y1.signum)
          rootBetween(x, y, x2, y2)
        else
          rootBetween(x, y, x1, y1)
      }
    }

  }

  // solution of 0 = (3n^2 - n - 2p) is integral
  def isPentagonal(p: Long): Boolean =
    Polynomial(List(-2*p, -1, 3)).rootBetween(0, p).isDefined

  case class Pentagonal(n: Long) {
    val p = pentagonal(n)
    lazy val step = p - Pentagonal(n-1).p
  }

  def pentagonal(n: Long): Long =
    n * (3*n-1) / 2

  def stream = Stream.from(1).map(Pentagonal(_)).iterator

  lazy val answer =
    stream.map(_.p).dropWhile(_ < 4300000).filter({ D =>
      println(D)// + " | " + stream.takeWhile(_.step <= D).last.p + " | " + Long.MaxValue)
      stream.takeWhile(_.step <= D).map(_.p)
        .exists({ a =>
          val b = a + D
          isPentagonal(b) && isPentagonal(a+b)
        })
    }).next()

}
