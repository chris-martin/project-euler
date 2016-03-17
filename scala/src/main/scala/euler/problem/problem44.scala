package euler.problem

package object problem44 {

  final case class Pentagonal(n: Int, p: Long) {
    def next: Pentagonal = Pentagonal(n+1, p + 3*(n+1) - 2)
  }

  lazy val pentagonals: Stream[Pentagonal] =
    Pentagonal(1, 1) #:: pentagonals.map(_.next)

  lazy val sumCandidates =
    (for (a <- pentagonals; b <- pentagonals.takeWhile(_.n < a.n)) yield (a, b))
      .filter(x => isPentagonal(x._1.p + x._2.p))

  def answer: Long =
    sumCandidates
      .filter(x => isPentagonal(x._1.p - x._2.p))
      .map(x => x._1.p - x._2.p)
      .head

  def pentagonal(n: Long): Long =
    n * (3*n-1) / 2

  // solution of 0 = (3n^2 - n - 2p) is integral
  def isPentagonal(p: Long): Boolean =
    Polynomial(List(-2*p, -1, 3)).rootBetween(0, p).isDefined

  final case class Polynomial(coeffs: List[Long]) {

    // http://rosettacode.org/wiki/Horner's_rule_for_polynomial_evaluation
    def apply(x: Long): Long =
      coeffs.reverse.foldLeft(0L){ (a, c) => a * x + c }

    def rootBetween(x1: Long, x2: Long): Option[Long] =
      rootBetween(x1, apply(x1), x2, apply(x2))

    @annotation.tailrec
    private def rootBetween(x1: Long, y1: Long, x2: Long, y2: Long): Option[Long] = {

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

}
