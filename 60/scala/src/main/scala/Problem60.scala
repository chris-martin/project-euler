import collection.mutable, mutable.ArrayBuffer

object Problem60 {

  object Primes {

    private val primeStream = Stream.from(2).filter(BigInt(_).isProbablePrime(30))
    private val primeSet = mutable.Set[Int]()
    private var unsearched: Stream[Int] = primeStream

    private def generateTo(i: Int) {
      while (unsearched.head <= i + 10000) {
        val i = unsearched.head
        unsearched = unsearched.tail
        primeSet add i
      }
    }

    def test(i: Int): Boolean =
      { generateTo(i); primeSet contains i }

    def iterator: Iterator[Int] = primeStream.iterator
  }

  // Graph: vertices are primes, edges are present where the concatenation property holds.
  // Find the 5-clique with the lowest sum.

  def isEdge(x: String, y: String): Boolean =
    Primes.test((x + y).toInt) &&
    Primes.test((y + x).toInt)

  def smallestCliqueOfSize(n: Int): Set[Int] = {
    val cliques = ArrayBuffer[ArrayBuffer[String]]()
    for (p <- Primes.iterator.map(_.toString)) {
      println(p)
      for (clique <- cliques) {
        if (clique forall (isEdge(p, _))) {
          clique += p
          if (clique.size == n) {
            return clique.map(_.toInt).toSet
          }
        }
      }
      cliques += ArrayBuffer(p)
    }
    throw new AssertionError
  }

  lazy val smallestCliqueOfSize4 = smallestCliqueOfSize(4)
  lazy val smallestCliqueOfSize5 = smallestCliqueOfSize(5)

  lazy val answer: Int = smallestCliqueOfSize(5).sum
}
