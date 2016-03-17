package euler.problem

import euler.util.prime.primes

package object problem21 {

  def answer: Int = answerMax(9999)

  type Factors = List[Int]

  // the proper divisors of the product of the factors
  def properDivisors(fs: Factors): Set[Int] =
    fs.indices.toSet.flatMap { n: Int =>
      fs.combinations(n).map(_.product)
    }

  def answerMax(max: Int): Int = {

    // prime factorizations of numbers in [2, max] in no particular order
    def factorizations(tail: Factors = Nil): Stream[Factors] =
      primes.map(_.intValue()).map(f => f :: tail match {
        case fs if fs.product > max => None
        case fs => Some(Stream.cons(fs, factorizations(fs)))
      }).takeWhile(_.isDefined).flatMap(_.get)

    // function : n in [2, max] -> Some(sum of proper divisors of n)
    lazy val divisorSums = {
      val ds = new Array[Int](max + 1)
      for (fs <- factorizations())
        ds.update(fs.product, properDivisors(fs).sum)
      i: Int => if (i >= 2 && i <= max) Some(ds(i)) else None
    }

    def isAmicable(i: Int): Boolean =
      divisorSums(i).exists(x => x != i && divisorSums(x) == Some(i))

    (2 to max).filter(isAmicable).sum
  }

}
