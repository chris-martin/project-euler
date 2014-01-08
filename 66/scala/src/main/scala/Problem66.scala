import scala.collection.mutable

object Problem66 {

  lazy val answer: Long = dMaxMin(maxD = 1000)

  case class Solution(d: Int, x: Long, y: Long) {
    assert(x*x - y*y*d == 1L, this)
  }

  def dMaxMin(maxD: Int): Int = {

    val unsolvedD = mutable.Map[Int, Double](
      (
        (1 to maxD).toSet &~
          Stream.from(1).map(x => x*x).takeWhile(_ <= maxD).toSet
      ).map(d => (d, math.sqrt(d))).toSeq: _*
    )

    val solutions = mutable.ArrayBuffer[Solution]()

    def addSolution(s: Solution) {
      solutions += s
      unsolvedD -= s.d
      println(s"${unsolvedD.size}\t$s")
    }

    var y = 1L
    var ySquared = 1L
    while (unsolvedD.nonEmpty) {
      for {
        (d, dRoot) <- unsolvedD
        x <- solveForXOptimized(y=y, ySquared=ySquared, d=d, dRoot=dRoot)
      } {
        addSolution(Solution(x=x, y=y, d=d))
      }
      ySquared += y + y + 1
      y += 1
    }

    solutions.maxBy(_.x).d
  }

  // x = sqrt( d*y*y + 1 )
  def solveForX(y: Long, d: Int): Option[Long] =
    solveForXOptimized(y=y, ySquared=y*y, d=d, dRoot = math.sqrt(d))

  def solveForXOptimized(y: Long, ySquared: Long, d: Int, dRoot: Double): Option[Long] = {
    val x = (y*dRoot).toLong + 1
    if (x*x == d*ySquared + 1) Some(x) else None
  }
}
