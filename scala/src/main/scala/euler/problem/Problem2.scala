package euler.problem

object Problem2 {

  lazy val answer: Int =
    fibs
    .takeWhile (_ < 4000000)
    .filter (_ % 2 == 0)
    .sum

  lazy val fibs: Stream[Int] =
    0 #::
    1 #::
    (fibs zip fibs.tail).map(Function tupled (_+_))

}
