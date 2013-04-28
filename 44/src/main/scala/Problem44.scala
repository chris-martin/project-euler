object Problem44 {

  case class Pentagonal(n: Long) {
    val p = n * (3*n-1) / 2
    lazy val step = p - Pentagonal(n-1).p
  }

  // http://rosettacode.org/wiki/Horner's_rule_for_polynomial_evaluation
  def horner(coeffs: List[Double], x: Double) =
    coeffs.reverse.foldLeft(0d){ (a, c) => a * x + c }

  def isPentagonal(p: Long): Boolean = {
    // 0 = (3n^2 - n - 2p)
    val n = horner(List(-2*p, -1, 3), p).round
    p == Pentagonal(n).p
  }

/*

  val isPentagonal = {

    val set = collection.mutable.HashSet[Int]()

    var maxN: Int = 0
    var maxP: Int = 0

    (p: Int) => {
      while (p > maxP) {
        maxN += 1
        maxP = Pentagonal(maxN).p
        set add maxP
        if (set.size % 1000 == 0) {
          println(set.size)
        }
      }
      set contains p
    }
  }
*/

  lazy val stream = Stream.from(1).map(Pentagonal(_))

  lazy val answer =
    stream.map(_.p).filter({ D =>
      println(D)
      stream.takeWhile(_.step <= D).map(_.p)
        .exists({ a =>
          val b = a + D
          isPentagonal(b) && isPentagonal(a+b)
        })
    }).head

}
