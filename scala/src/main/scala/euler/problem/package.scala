package euler

import euler.util.Bag

import scala.collection.mutable

import scalaz._, Scalaz._, effect._, Maybe._

import euler.util.fibonacci.fibs
import euler.util.palindrome.palindromeInt
import euler.util.prime.{primes, factorize}

package object problem {

  def inputString(n: Int): IO[String] =
    IO { io.Source.fromFile(s"../problems/$n-data.txt").mkString }

  def answer(n: Int): IO[Maybe[String]] = answerPartial.lift(n) match {
    case None => empty[String].pure[IO]
    case Some(x) => x.map(just)
  }

  val answerPartial: PartialFunction[Int, IO[String]] = {

    case 1 =>
      (1 to 999).filter(i => (i % 3 == 0) || (i % 5 == 0))
        .sum.toString.pure[IO]

    case 2 =>
      fibs.takeWhile (_ < 4000000).filter (_ % 2 == 0)
        .sum.toInt.toString.pure[IO]

    case 3 =>
      var x = BigInt("600851475143")
      while (!x.isProbablePrime(40))
        x /= primes.filter(i => (x mod i) == 0).head
      x.toInt.toString.pure[IO]

    case 4 =>
      (1 to 999).map(BigInt(_)).combinations(2)
        .map(x => x(0) * x(1)).filter(palindromeInt(_))
        .max.intValue.toString.pure[IO]

    case 5 =>
      Bag.union((2 to 20).map(factorize(_))).toSeq
        .product.toString.pure[IO]

    case 6 =>
      def square(x: Int): Int = x * x
      (square((1 to 100).sum) - (1 to 100).map(square).sum)
        .toString.pure[IO]

    case 7 =>
      primes(10000).intValue.toString.pure[IO]

    case 8 => for (input <- inputString(8)) yield
      input.split("\n").map(_.trim)
        .flatMap(_.map(_.toString.toInt))
        .sliding(5).map(_.product).max
        .toString

    case 9 =>
      def square(n: Int) = n * n
      (for {
        a <- 0 to 1000
        b <- 0 to a
        c = 1000 - (a + b)
        if square(a) + square(b) == square(c)
      } yield a * b * c).head.toString.pure[IO]

    case 10 =>
      primes.takeWhile(_ <= 2000000).sum.toString.pure[IO]

    case 11 =>
      for (input <- inputString(11)) yield
        problem11.answer(input).toString

    case 12 =>
      problem12.answer.toString.pure[IO]

    case 13 =>
      for (input <- inputString(13)) yield
        input.split("\n").map(_.trim).filter(_.nonEmpty)
          .map(BigInt(_)).sum.toString.substring(0, 10)

    case 14 =>
      problem14.answer.toString.pure[IO]

    case 15 =>
      val memo = collection.mutable.HashMap[(Int, Int), Long]()

      // how many n-bit strings have weight w?
      def count(n: Int, w: Int): Long =
        if (w < 0 || w > n)
          0
        else if (w == 0 || w == n)
          1
        else if (memo.contains((n, w)))
          memo((n, w))
        else {
          val c = count(n-1, w) + count(n-1, w-1)
          memo.put((n, w), c)
          c
        }

      count(40, 20).toString.pure[IO]

    case 16 =>
      BigInt(2).pow(1000).toString(10)
        .map(_.toString.toInt).sum.toString.pure[IO]

    case 17 =>
      problem17.answer.toString.pure[IO]

    case 18 =>
      problem18.answer.toString.pure[IO]

    case 19 =>
      problem19.answer.toString.pure[IO]

    case 20 =>
      (1 to 100).map(BigInt(_)).product.toString
        .map(_.toString.toInt).sum.toString.pure[IO]

    case 21 =>
      problem21.answer.toString.pure[IO]

    case 22 =>
      def unquote(s: String): String =
        s.stripPrefix("\"").stripSuffix("\"")
      for (input <- inputString(22)) yield
        input.toUpperCase.split(",")
          .map(_.trim).map(unquote).sorted
          .map(name => name.map(_ - 'A' + 1).sum)
          .zip(Stream.from(1))
          .map({ case (x, y) => x * y }).sum.toString

    case 23 =>
      problem23.answer.toString.pure[IO]

    case 24 =>
      ('0' to '9').permutations.drop(999999)
        .next().mkString.pure[IO]

    case 25 =>
      fibs.indexWhere(_.toString.length >= 1000)
        .toString.pure[IO]

    case 26 =>
      problem26.answer.toString.pure[IO]

    case 27 =>
      problem27.answer.toString.pure[IO]

    case 28 =>
      def square(n: Int): Int = n*n
      (1 + (1 to 500).flatMap(i => {
        val j = 2*i
        val x = square(j+1)
        Seq(x, x-j, x-2*j, x-3*j)
      }).sum).toString.pure[IO]

    case 29 =>
      (for { a <- 2 to 100; b <- 2 to 100 } yield
        BigInt(a) pow b).toSet.size.toString.pure[IO]

    case 30 =>
      problem30.answer.toString.pure[IO]

    case 31 =>
      problem31.answer.toString.pure[IO]

    case 32 =>
      problem32.answer.toString.pure[IO]

    case 33 =>
      problem33.answer.toString.pure[IO]

    case 34 =>

      lazy val factorials: Stream[BigInt] =
        BigInt(1) #:: Stream.from(1).zip(factorials).map(x => x._2 * x._1)

      val maxDigits = Stream.from(1).takeWhile({ i =>
        (factorials(9) * i) >= (BigInt(10) pow i)
      }).last

      def curious(n: Int): Boolean =
        n == n.toString.map(_.toString.toInt).map(factorials(_)).sum

      (3 to BigInt(10).pow(maxDigits).toInt).filter(curious).sum
        .toString.pure[IO]

    case 35 =>
      (2 to 999999).count({ x =>
        val s = x.toString
        (0 to s.length) forall { i =>
          val t = s.substring(i) + s.substring(0, i)
          BigInt(t).isProbablePrime(40)
        }
      }).toString.pure[IO]

    case 36 =>
      (1 to 999999).map(BigInt(_))
        .filter(palindromeInt(_, base=10))
        .filter(palindromeInt(_, base=2))
        .sum.toInt.toString.pure[IO]

    case 37 =>
      problem37.answer.toString.pure[IO]

    case 38 =>
      problem38.answer.toString.pure[IO]

    case 39 =>
      problem39.answer.toString.pure[IO]

    case 40 =>
      val d: Stream[Int] = Stream.from(1)
        .flatMap(_.toString.map(_.toString.toInt))
      (0 to 6).map(i => d(BigInt(10).pow(i).toInt - 1))
        .product.toString.pure[IO]

    case 41 =>
      (1 to 9)
        .flatMap(i => (1 to i).permutations)
        .map(_.mkString).sorted.reverse
        .filter(BigInt(_).isProbablePrime(40))
        .head
        .toInt.toString.pure[IO]

    case 42 =>
      val triangles =
        Stream.from(1).map(n => n * (n + 1) / 2)
      for (input <- inputString(42)) yield
        input.replace("\"", "").split(",")
          .map(_.toUpperCase.map(_ - 'A' + 1).sum)
          .count(v => triangles.takeWhile(_ <= v).contains(v))
          .toString

    case 43 =>
      (0 to 9).permutations.map(_.mkString)
        .filter({ s =>
          s.substring(1, 4 ).toInt %  2 == 0 &&
          s.substring(2, 5 ).toInt %  3 == 0 &&
          s.substring(3, 6 ).toInt %  5 == 0 &&
          s.substring(4, 7 ).toInt %  7 == 0 &&
          s.substring(5, 8 ).toInt % 11 == 0 &&
          s.substring(6, 9 ).toInt % 13 == 0 &&
          s.substring(7, 10).toInt % 17 == 0
        })
        .map(_.toLong).sum.toString.pure[IO]

    case 44 =>
      problem44.answer.toString.pure[IO]

    case 45 =>
      problem45.answer.toString.pure[IO]

    case 46 =>
      problem46.answer.toString.pure[IO]

    case 47 =>
      var bound = 200000
      (Stream continually {
        val factorCount = mutable.ArrayBuffer.fill(bound)(0)
        primes.map(_.intValue).takeWhile(_ < bound) foreach { prime =>
          (Stream from 1).map(_ * prime).takeWhile(_ < bound) foreach { multiple =>
            factorCount(multiple) += 1
          }
        }
        bound *= 2
        factorCount.zipWithIndex.sliding(4).find(_.forall(_._1 == 4)).map(_.head._2)
      }).flatten.head.toString.pure[IO]

    case 48 =>
      (1 to 1000).map(i => BigInt(i) pow i)
        .sum.toString.takeRight(10).mkString.toLong.toString.pure[IO]

    case 49 =>
      (1000 to 9999).filter(BigInt(_).isProbablePrime(32))
        .groupBy(_.toString.sorted.mkString).values
        .flatMap(_.sorted.combinations(3))
        .filter(x => x(2) - x(1) == x(1) - x(0))
        .filterNot(_ == Seq(1487, 4817, 8147))
        .head.mkString.pure[IO]

    case 50 =>
      val primeSeq: Seq[Int] = primes.takeWhile(_ <= 999999).map(_.intValue).toSeq
      val primeSet: Set[Int] = primeSeq.toSet

      (0 until Stream.from(1).takeWhile(primeSeq.take(_).sum <= primeSeq.last).last)
        .reverseIterator
        .flatMap({ length =>
          (0 to primeSeq.length - length).map(a => primeSeq.slice(a, a + length).sum)
        })
        .filter(primeSet.contains).next().toString.pure[IO]

    case 51 =>
      problem51.answer.toString.pure[IO]

    case 52 =>
      problem52.answer.toString.pure[IO]

    case 53 =>
      implicit class RichInt(n: Int) {
        def factorial: BigInt =
          (2 to n).foldLeft(BigInt(1))(_*_)
        def choose(r: Int): BigInt =
          n.factorial / ( r.factorial * (n-r).factorial )
      }
      (1 to 100).flatMap(n =>
        (1 to n).map(r => n choose r)).count(_ > 1000000).toString.pure[IO]

    case 54 =>
      problem54.answer.toString.pure[IO]

    case 55 =>
      ((0 until 10000) count { n =>
        lazy val iterations: Stream[BigInt] = BigInt(n) #::
          ( iterations map { i => i + BigInt(i.toString.reverse) } )
        !(
          iterations.slice(1, 50) exists { i =>
            val s = i.toString
            s == s.reverse
          }
        )
      }).toString.pure[IO]

    case 56 =>
      (
        for (a <- 1 to 99; b <- 1 to 99)
        yield (BigInt(a) pow b).toString.map(_.toString.toInt).sum
      ).max.toString.pure[IO]

    case 57 =>
      problem57.answer.toString.pure[IO]

    case 58 =>
      problem58.answer.toString.pure[IO]

    case 59 =>
      for (input <- inputString(59)) yield
        problem59.answer(input).toString

    case 60 =>
      problem60.answer.toString.pure[IO]

    case 61 =>
      problem61.answer.toString.pure[IO]

    case 62 =>
      problem62.answer.toString.pure[IO]

    case 63 =>
      problem63.answer.toString.pure[IO]

    case 64 =>
      problem64.answer.toString.pure[IO]

    case 65 =>
      problem65.answer.toString.pure[IO]

  }

}
