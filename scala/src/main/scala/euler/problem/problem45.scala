package euler.problem

package object problem45 {

  def answer = greatNumbers.dropWhile(_ <= 40755).head

  val hexagonals = Stream.from(1).map({ n => n*(2*n-1) })

  val greatNumbers = hexagonals.filter({ z =>
    isPentagonal(z) && isTriangle(z)
  })

  // 0 = -2z + n + n^2
  def isTriangle(z: Long): Boolean =
    Polynomial(List(-2*z, 1, 1)).rootBetween(0, z).isDefined

  // 0 = -2z - n + 3n^2
  def isPentagonal(z: Long): Boolean =
    Polynomial(List(-2*z, -1, 3)).rootBetween(0, z).isDefined

  // 0 = -2z -n + 2n^2
  def isHexagonal(z: Long): Boolean =
    Polynomial(List(-z, -1, 2)).rootBetween(0, z).isDefined

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
