package euler.problem

package object problem58 {

  final case class Fraction(a: BigInt, b: BigInt) {
    def reduce: Fraction = { val d = a gcd b; Fraction(a/d, b/d) }
    def +(f: Fraction): Fraction = Fraction(a * f.b + f.a * b, b * f.b).reduce
    def ++(f: Fraction): Fraction = Fraction(a + f.a, b + f.b)
    def unary_- : Fraction = Fraction(-a, b)
    def -(f: Fraction): Fraction = this + (-f)
    def <(f: Fraction): Boolean = (this - f).sign < 0
    def sign: Int = if (a == 0) 0 else if (a.signum == b.signum) 1 else -1
  }

  final case class Ring(index: Int, corners: Seq[Int]) {

    def next: Ring = Ring(
      index = index + 1,
      corners = for (s <- 1 to 4) yield corners.last + s * (sideLength + 1)
    )

    def sideLength = 2 * index + 1

    def primeFraction: Fraction = Fraction(
      corners.count(BigInt(_).isProbablePrime(32)),
      corners.size
    )

  }

  val center = Ring(0, 1::Nil)

  lazy val rings: Stream[Ring] = center #:: rings.map(_.next)

  lazy val runningFraction: Stream[(Ring, Fraction)] =
    (center, center.primeFraction) #::
      runningFraction.map({
        case (ring, fraction) =>
          val next = ring.next
          (next, fraction ++ next.primeFraction)
      })

  def answer: Int =
    runningFraction.drop(1).find({
      case (_, fraction) => fraction < Fraction(1, 10)
    }).map({
      case (ring, _) => ring.sideLength
    }).get

}
