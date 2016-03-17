package euler.problem

package object problem57 {

  final case class Fraction(a: BigInt, b: BigInt) {
    def reduce: Fraction = { val d = a gcd b; Fraction(a/d, b/d) }
    def inverse: Fraction = Fraction(b, a)
    def +(f: Fraction): Fraction = Fraction(a * f.b + f.a * b, b * f.b).reduce
  }

  object Fraction {
    implicit def apply(a: Long): Fraction = Fraction(a, 1)
  }

  lazy val iterations: Stream[Fraction] =
    Fraction(3, 2) #:: ( iterations map { n => 1 + (1 + n).inverse } )

  implicit class RichBigInt(x: BigInt) {
    def digits: Int = x.toString.length
  }

  def answer: Int =
    iterations.slice(1, 1001) count { f => f.a.digits > f.b.digits }

}
