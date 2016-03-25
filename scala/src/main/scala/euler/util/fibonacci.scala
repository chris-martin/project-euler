package euler.util

package object fibonacci {

  lazy val fibs: Stream[BigInt] =
    0 #::
    1 #::
    (fibs zip fibs.tail).map({ case (a, b) => a + b })

}
