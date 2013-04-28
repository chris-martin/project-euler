object Problem12 {

  lazy val primes: Stream[BigInt] =
    Stream.from(2).map(BigInt(_)).filter(_.isProbablePrime(45))

  lazy val triangles: Stream[BigInt] = BigInt(0) #::
    triangles.zip(Stream.from(1)).map(x => x._1 + x._2)

  def nrOfFactors(n: BigInt): Int = {
    val fs = new collection.mutable.HashMap[BigInt, Int]
    var i = n
    while (i != 1) {
      val p = primes.find(p => i % p == 0).get
      fs.put(p, fs.getOrElse(p, 0) + 1)
      i /= p
    }
    fs.values.map(1 + _).product
  }

  lazy val answer: Int =
    triangles.drop(1).filter(nrOfFactors(_) > 500).head.toInt

}
