import scala.collection.mutable

object Problem66 {

  lazy val answer: Int = dMaximizingX(maxD = 1000)

  object Roots {

    val map = mutable.Map[Long, Int]()
    var maxSquare = 0L
    var maxRoot = 0

    def get(x: Long): Option[Int] = {
      while (maxSquare < x) {
        maxRoot += 1
        maxSquare = maxRoot * maxRoot
        map put (maxSquare, maxRoot)
      }
      map get x
    }
  }

  def dMaximizingX(maxD: Int): Int = {

    val unsolvedD = mutable.Set[Int]((
      (1 to maxD).toSet &~
        Stream.from(1).map(x => x*x).takeWhile(_ <= maxD).toSet
    ).toSeq :_*)

    var x = 2
    var lastD = 0
    while (unsolvedD.nonEmpty) {
      for (d <- unsolvedD) {
        solveForY(x = x, d = d) foreach { y =>
          lastD = d
          unsolvedD -= d
          println(s"${unsolvedD.size}\td = $d  \tx = $x\ty = $y")
        }
      }
      x += 1
    }

    lastD
  }

  def solveForY(x: Int, d: Int): Option[Int] =
    divide(x*x - 1, d) flatMap (Roots.get(_))

  def divide(a: Int, b: Int): Option[Int] =
    BigInt(a) /% b match {
      case (q, r) if r.toInt == 0 => Some(q.toInt)
      case _ => None
    }
}
