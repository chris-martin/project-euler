package euler.problem

import euler.util.prime.primes

object Problem23 {

  val max = 28123

  type Factors = List[Int]

  // prime factorizations of numbers in [2, max] in no particular order
  def factorizations(tail: Factors = Nil): Stream[Factors] =
    primes.map(_.intValue).map(f => f :: tail match {
      case fs if fs.product > max => None
      case fs => Some(Stream.cons(fs, factorizations(fs)))
    }).takeWhile(_.isDefined).flatMap(_.get)

  // the proper divisors of the product of the factors
  def properDivisors(fs: Factors): Set[Int] = {
    fs.indices.toSet.flatMap { n: Int =>
      fs.combinations(n).map(_.product)
    }
  }

  lazy val answer: Int = {

    // function : n in [2, max] -> sum of proper divisors of n
    println("generating divisor sums")
    val divisorSum = {
      val ds = new Array[Int](max + 1)
      for (fs <- factorizations()) {
        ds.update(fs.product, properDivisors(fs).sum)
      }
      ds
    }

    println("generating abundant numbers")
    val abundant = Stream.range(2, max).filter(n => divisorSum(n) > n).toArray

    println("initializing X")
    val X = collection.mutable.HashSet(1 to max: _*)

    println("filtering X")
    for (x <- 0 until abundant.length; y <- x until abundant.length) {
      X -= (abundant(x) + abundant(y))
    }

    X.sum

  }

}
