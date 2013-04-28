object Problem23 extends App {

  println(answer)

  def answer: Int = {

    val max = 28123

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
    val X = collection.mutable.HashSet((1 to max): _*)

    println("filtering X")
    for (x <- 0 until abundant.size; y <- x until abundant.size) {
      X -= (abundant(x) + abundant(y))
    }

    X.sum

  }

}
