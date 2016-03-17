package euler.problem

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

package object problem60 {

  val primality = mutable.Map[Long, Boolean]()
    .withDefault(BigInt(_).isProbablePrime(30))

  // Graph: vertices are primes, edges are present where the
  // concatenation property holds. Find the 5-clique with
  // the lowest sum.

  def isEdge(x: Long, y: Long): Boolean =
    isEdge(x.toString, y.toString)

  def isEdge(x: String, y: String): Boolean =
    primality((x + y).toLong) &&
    primality((y + x).toLong)

  class Clique(vs: Seq[Long]) {
    def sum: Long = vs.sum
    def accepts(p: Long): Boolean = vs forall (isEdge(p, _))
    def :+(p: Long): Clique = new Clique(vs :+ p)
    def size: Int = vs.size
    def toSet: Set[Long] = vs.toSet
    override def toString = s"${vs.mkString(" + ")} = ${vs.sum}"
  }
  object Clique {
    def apply(ps: Long*): Clique = new Clique(ArrayBuffer(ps: _*))
  }

  class HashCodeOrdering[A] extends Ordering[A] {
    def compare(x: A, y: A) = x.hashCode - y.hashCode
  }

  def smallestCliqueOfSize(n: Int, maxSum: Long): Option[Set[Long]] = {

    val cliques = mutable.TreeSet[Clique]()(new HashCodeOrdering)
    var result: Option[Clique] = None

    for (p <- Iterator.iterate(2L)(_+1)
                .takeWhile(_ <= maxSum).filter(primality)) {
      for (clique <- cliques) {
        if (clique.sum + p >= maxSum) {
          cliques -= clique
        }
        if (clique accepts p) {
          val newClique = clique :+ p
          if (newClique.size == n) {
            result = Some((Seq(newClique) ++ result).minBy(_.sum))
          } else {
            cliques += newClique
          }
        }
      }
      cliques += Clique(p)
    }
    result.map(_.toSet)
  }

  lazy val smallestCliqueOfSize4 = smallestCliqueOfSize(4, 1000)
  lazy val smallestCliqueOfSize5 = smallestCliqueOfSize(5, 27000)

  def answer: Long = smallestCliqueOfSize5.get.sum

}
