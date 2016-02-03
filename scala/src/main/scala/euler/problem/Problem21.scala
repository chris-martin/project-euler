package euler.problem

import euler.util.prime.primes

object Problem21 {

  lazy val answer: Int = (2 to max).filter(isAmicable).sum

  lazy val max = 9999

  type Factors = List[Int]

  // prime factorizations of numbers in [2, max] in no particular order
  def factorizations(tail: Factors = Nil): Stream[Factors] =
    primes.map(_.intValue())
      .map(_ :: tail)
      .map(fs => if (fs.product > max) None
                 else Some(fs #:: factorizations(fs)))
      .takeWhile(_.isDefined)
      .flatMap(_.get)

  // the proper divisors of the product of the factors
  def properDivisors(fs: Factors): Set[Int] =
    fs.indices.toSet.flatMap { n: Int =>
      fs.combinations(n).map(_.product)
    }

  // function : n in [2, max] -> Some(sum of proper divisors of n)
  lazy val divisorSums: (Int) => Option[Int] = {
    val ds = new Array[Int](max + 1)
    for (fs <- factorizations())
      ds.update(fs.product, properDivisors(fs).sum)
    i: Int => if (i >= 2 && i <= max) Some(ds(i)) else None
  }

  def isAmicable(i: Int): Boolean =
    divisorSums(i).exists(x => x != i && divisorSums(x) == Some(i))

}
