package euler.problem

import scala.collection.{immutable, mutable}

package object problem62 {

  implicit class RichIterator[A](it: Iterator[A]) {

    def contiguouslyGroupBy[B](f: A => B): Iterator[Seq[A]] = new Iterator[Seq[A]] {

      var peek: Option[A] = None

      def hasNext = peek.nonEmpty || it.hasNext

      def next(): Seq[A] = {
        if (!hasNext) throw new NoSuchElementException
        if (peek.isEmpty) peek = Some(it.next())
        val group = mutable.ArrayBuffer[A](peek.get)
        val key = f(peek.get)
        peek = None
        while (it.hasNext && peek.isEmpty) {
          val x = it.next
          if (f(x) != key)
            peek = Some(x)
          else
            group += x
        }
        immutable.Seq(group: _*)
      }
    }
  }

  implicit class RichIterable[A](it: Iterable[A]) {
    def minOption(implicit ord: Ordering[A]): Option[A] =
      if (it.isEmpty) None else Some(it.min)
  }

  def permutationCubes(permutations: Int): BigInt =
    Iterator
      .iterate(BigInt(1))(_+1).map(i => i*i*i)
      .map(_.toString)
      .contiguouslyGroupBy(_.length)
      .flatMap({ cubes =>
        cubes.groupBy(_.sorted.mkString).values
          .filter(_.size == permutations).map({ cubes =>
            cubes.map(BigInt(_)).min
          })
          .minOption
      })
      .next()

  def answer: BigInt = permutationCubes(5)

}
