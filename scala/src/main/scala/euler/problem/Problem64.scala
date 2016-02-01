package euler.problem

import scala.math.sqrt

object Problem64 {

  lazy val answer: Int = countOddPeriods(10000)

  // number of continued fractions ≤ n with an odd period
  def countOddPeriods(n: Int): Int =
    (2 to n).count(continuedFraction(_).period % 2 == 1)

  def continuedFraction(n: Int): ContinuedFraction = {
    val base = sqrtFloor(n)
    if (square(base) == n) {
      ContinuedFraction(base, Nil)
    } else {
      lazy val expressions: Stream[Expression3] =
        Expression1(n = n, a = 1, b = -base).next.next #::
        expressions.map(_.next.next.next).takeWhile(_ != expressions.head)
      ContinuedFraction(base, expressions.map(_.a).force)
    }
  }

  case class ContinuedFraction(base: Int, repetition: Seq[Int]) {
    def period: Int = repetition.size
  }

  case class Expression1(n: Int, a: Int, b: Int) {
    lazy val value = a / ( sqrt(n) + b )
    lazy val next = Expression2(
      n = n,
      a = a,
      b = -b,
      c = n - square(b)
    )
  }

  case class Expression2(n: Int, a: Int, b: Int, c: Int) {
    lazy val value = a * ( sqrt(n) + b ) / c
    lazy val next = Expression3(
      n = n,
      a = floor(value),
      b = b - floor(value) * (c/a),
      c = c/a
    )
  }

  case class Expression3(n: Int, a: Int, b: Int, c: Int) {
    lazy val value = a + ( sqrt(n) + b ) / c
    lazy val next = Expression1(
      n = n,
      a = c,
      b = b
    )
  }

  def sqrtFloor(x: Int): Int = {
    val root = sqrt(x)
    val roundedRoot = root.round.toInt
    if (square(roundedRoot) == x)
      roundedRoot
    else
      floor(root)
  }

  def floor(x: Double): Int = x.toInt

  def square[N: Numeric](x: N) =
    implicitly[Numeric[N]].times(x, x)
}
