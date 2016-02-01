package euler.problem

import scala.collection.{immutable, mutable}

object Problem51 {

  lazy val primes: Stream[Int] =
    Stream.from(2).filter(BigInt(_).isProbablePrime(32))

  /** A collection of primes that have the same number of digits. */
  case class PrimeDigitGroup(members: Seq[Int]) {

    lazy val families: Seq[PrimeValueFamily] =
      members.map(_.toString).groupByMultiple(_.starPossibilities).values.toSeq map {
        primeStrings => PrimeValueFamily(primeStrings.toSeq.map(_.toInt))
      }

  }

  /** A prime sequence such as [56003, 56113, 56333, 56443, 56663, 56773, 56993]. */
  case class PrimeValueFamily(members: immutable.SortedSet[Int]) {

    def size: Int = members.size

  }

  object PrimeValueFamily {

    def apply(members: Seq[Int]): PrimeValueFamily =
      PrimeValueFamily(immutable.SortedSet(members: _*))

    implicit val primeValueFamilyOrdering: Ordering[PrimeValueFamily] =
      Ordering.by(_.members.toSeq(0))

  }

  lazy val primeDigitGroups: Stream[PrimeDigitGroup] =
    Stream.from(0) map { digits => PrimeDigitGroup(
      primes.map(_.toString)
      .dropWhile(_.length < digits)
      .takeWhile(_.length == digits)
      .map(_.toInt)
    ) }

  implicit class RichIterable[A](it: Iterable[A]) {

    def groupByMultiple[B](f: A => Iterable[B]): Map[B, Iterable[A]] = {
      val map = mutable.Map[B, mutable.Builder[A, Iterable[A]]]()
      it.foreach(a => f(a).foreach(b => map.getOrElseUpdate(b, it.genericBuilder) += a))
      Map(map.mapValues(_.result()).toSeq: _*)
    }

  }

  implicit class RichSeq[A](seq: Seq[A]) {

    def replace(map: Map[Int, A]): Seq[A] =
      seq.zipWithIndex.map(x => map.getOrElse(x._2, x._1))

    def replacementPossibilities(oldValue: A, newValue: A): Iterable[Seq[A]] = {
      seq.zipWithIndex.filter(_._1 == oldValue).map(_._2).toSet.subsets.toIterable
        .map({ indices: Iterable[Int] => seq.replace(indices.map( i => (i, newValue)).toMap) })
    }

  }

  implicit class RichString(s: String) {

    def starPossibilities: Seq[String] =
      s.toSeq.distinct.flatMap({ character: Char =>
        s.toSeq.replacementPossibilities(character, '*').map(_.mkString).filterNot(_ == s)
      })

  }

  def smallestPrimeValueFamily(familySize: Int): PrimeValueFamily =
    primeDigitGroups.flatMap(_.families.filter(_.size >= familySize).sorted).head

  lazy val answer: Int = smallestPrimeValueFamily(8).members.min

}
