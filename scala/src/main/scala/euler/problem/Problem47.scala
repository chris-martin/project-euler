package euler.problem

import scala.collection.mutable

import euler.util.prime.primes

object Problem47 {

  lazy val answer: Long = {

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

    }).flatten.head

  }

}
