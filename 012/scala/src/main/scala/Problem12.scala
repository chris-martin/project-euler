object Problem12 {

  println("hi")

  lazy val primes: Stream[BigInt] =
    Stream.iterate(BigInt(2))(_ + 1)
      .filter(_.isProbablePrime(45))

  lazy val triangles: Stream[BigInt] =
    Stream.iterate(BigInt(1))(_ + 1)
      .scanLeft(BigInt(0))(_ + _)

  def nrOfFactors(n: BigInt): Int = {
    val fs = new collection.mutable.HashMap[BigInt, Int]
    var i = n
    while (i != BigInt(1)) {
      val p = primes.find(p => i % p == 0).get
      fs.put(p, fs.getOrElse(p, 0) + 1)
      i /= p
    }
    fs.values.map(1 + _).product
  }

  lazy val answer: Int =
    triangles.drop(1).filter(nrOfFactors(_) > 500).head.toInt

}
