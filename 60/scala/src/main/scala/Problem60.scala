import collection.{mutable, immutable}

// Graph: vertices are primes, edges are present where the concatenation property holds.
// Find the 5-clique with the lowest sum.

object Problem60 {

  val primality = mutable.Map[Long, Boolean]().withDefault(BigInt(_).isProbablePrime(30))

  def smallestCliqueOfSize(n: Int, maxSum: Long): Set[Long] = {

    println(n)

    val primes = Iterator.iterate(2L)(_+1).takeWhile(_ <= maxSum).filter(BigInt(_).isProbablePrime(30)).toSet

    def isEdge(x: Long, y: Long): Boolean =
      isEdgeString(x.toString, y.toString)

    def isEdgeString(x: String, y: String): Boolean =
      primality((x + y).toLong) &&
      primality((y + x).toLong)

    val vertexAdjacencies = mutable.Map[Long, Set[Long]]().withDefault(p => primes.filter(q => isEdge(p, q)))

    class Clique(vs: Set[Long]) {
      lazy val sum: Long = vs.sum
      def accepts(p: Long): Boolean = vs forall (isEdge(p, _))
      def +(p: Long): Clique = new Clique(vs + p)
      def size: Int = vs.size
      def toSet: Set[Long] = vs.toSet
      def contains(x: Long) = vs contains x
      def adjacencies = vs.flatMap(vertexAdjacencies)
      override def toString = s"${vs.mkString(" + ")} = ${vs.sum}"
    }
    object Clique {
      def apply(ps: Long*): Clique = new Clique(Set(ps: _*))
    }

    var cliques = primes.map(Clique(_)).par

    for (i <- 2 to n) {
      println(s"-- $i")
      cliques = cliques.flatMap({ clique =>
        clique.adjacencies.filter(_ + clique.sum <= maxSum).filter(clique accepts _).map(clique + _)
      })
    }

    cliques.minBy(_.sum).toSet
  }

  lazy val smallestCliqueOfSize4 = smallestCliqueOfSize(4, 1000)
  lazy val smallestCliqueOfSize5 = smallestCliqueOfSize(5, 27000)

  lazy val answer: Long = smallestCliqueOfSize5.sum
}
