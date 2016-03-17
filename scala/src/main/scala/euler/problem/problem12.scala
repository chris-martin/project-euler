package euler.problem

import euler.util.prime.primes

import scala.annotation.tailrec

package object problem12 {

  def triangles: Stream[BigInt] =
    Stream.iterate(BigInt(1))(_ + 1)
      .scanLeft(BigInt(0))(_ + _)

  @tailrec
  def nrOfFactors(n: BigInt, fs: Map[BigInt, Int] = Map.empty): Int =
    if (n == BigInt(1)) {
      fs.values.map(1 + _).product
    } else {
      val p = primes.find(p => n % p == 0).get
      nrOfFactors(n / p, fs.updated(p, fs.getOrElse(p, 0) + 1))
    }

  def answer: Int =
    triangles.drop(1).filter(nrOfFactors(_) > 500).head.toInt

}
