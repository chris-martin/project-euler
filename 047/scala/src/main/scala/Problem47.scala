import scala.collection.mutable

object Problem47 {

  lazy val primes: Stream[Int] = Stream.from(2).filter(BigInt(_).isProbablePrime(32))

  lazy val answer: Long = {

    var bound = 200000

    (Stream continually {

      val factorCount = mutable.ArrayBuffer.fill(bound)(0)

      primes.takeWhile(_ < bound) foreach { prime =>
        (Stream from 1).map(_ * prime).takeWhile(_ < bound) foreach { multiple =>
          factorCount(multiple) += 1
        }
      }

      bound *= 2

      factorCount.zipWithIndex.sliding(4).find(_.forall(_._1 == 4)).map(_.head._2)

    }).flatten.head

  }

}
