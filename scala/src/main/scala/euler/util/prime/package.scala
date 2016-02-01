package euler.util

import java.math.BigInteger

import euler.util.bag._

import scala.annotation.tailrec

package object prime {

  def divides(x: Int, y: Int): Boolean = y % x == 0

  def isPrime(n: Int): Boolean =
    BigInteger.valueOf(n).isProbablePrime(10)

  val primes: Stream[Int] =
    Stream.from(1).filter(isPrime)

  def smallestFactor(n: Int): Int =
    primes.filter(divides(_, n)).head

  @tailrec
  final def factorize(n: Int, agg: Bag[Int] = Bag.empty): Bag[Int] =
    if (n == 1) agg
    else {
      val f = smallestFactor(n)
      factorize(n / f, agg :+ f)
    }

}
