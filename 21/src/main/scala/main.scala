object Main {
  def main(args: Array[String]) {

    val max = 9999

    val primes: Stream[Int] = Stream.from(2).filter(BigInt(_).isProbablePrime(45))

    type Factors = List[Int]

    // prime factorizations of numbers in [2, max] in no particular order
    def factorizations(tail: Factors = Nil): Stream[Factors] =
      primes.map(f => f :: tail match {
        case fs if fs.product > max => None
        case fs => Some(Stream.cons(fs, factorizations(fs)))
      }).takeWhile(_.isDefined).map(_.get).flatten

    // the proper divisors of the product of the factors
    def properDivisors(fs: Factors): Set[Int] = {
      (0 until fs.size).toSet.flatMap { n: Int =>
        fs.combinations(n).map(_.product)
      }
    }

    // function : n in [2, max] -> Some(sum of proper divisors of n)
    val divisorSums = {
      val ds = new Array[Int](max + 1)
      for (fs <- factorizations()) {
        ds.update(fs.product, properDivisors(fs).sum)
      }
      i: Int => if (i >= 2 && i <= max) Some(ds(i)) else None
    }

    def isAmicable(i: Int): Boolean =
      divisorSums(i).map(x => x != i && divisorSums(x) == Some(i)).getOrElse(false)

    println((2 to max).filter(isAmicable(_)).sum)

  }
}
