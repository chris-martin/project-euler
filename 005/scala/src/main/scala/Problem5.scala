import java.math.BigInteger

import scala.annotation.tailrec
import scala.collection.immutable.Map

object Problem5 {

  // generic utils

  def divides(x: Int, y: Int): Boolean = y % x == 0

  // bags

  /** Bags are represented as map from item to cardinality */
  type Bag[A] = Map[A, Int]

  /** An empty bag */
  def bagEmpty[A]: Bag[A] = Map.empty

  /** How many elem are in the bag? */
  def bagCardinality[A](bag: Bag[A], elem: A): Int =
    bag.getOrElse(elem, 0)

  /** Add one elem to the bag */
  def bagAdd[A](bag: Bag[A], elem: A): Bag[A] =
    bag.updated(elem, 1 + bagCardinality(bag, elem))

  /** Union of any number of bags */
  def bagUnion[A](bags: Seq[Bag[A]]): Bag[A] =
    Map[A, Int]((for {
      elem <- bags.map(_.keySet).foldLeft[Set[A]](Set.empty)(_ union _).toSeq
      cardinality = bags.map(bag => bagCardinality(bag, elem)).max
    } yield (elem: A, cardinality: Int)): _*)

  def bagToSeq[A](bag: Bag[A]): Seq[A] =
    bag.flatMap({
      case (elem, cardinality) => Seq.fill(cardinality)(elem)
    }).toSeq

  // primes

  def isPrime(n: Int): Boolean =
    BigInteger.valueOf(n).isProbablePrime(10)

  val primes: Stream[Int] =
    Stream.from(1).filter(isPrime)

  def smallestFactor(n: Int): Int =
    primes.filter(divides(_, n)).head

  /** Get a bag of prime factors */
  @tailrec
  def factorize(n: Int, agg: Bag[Int] = bagEmpty): Bag[Int] =
    if (n == 1) agg
    else {
      val f = smallestFactor(n)
      factorize(n / f, bagAdd(agg, f))
    }

  // answer

  lazy val answer: Int =
    bagToSeq(bagUnion((2 to 20).map(factorize(_)))).product

}
