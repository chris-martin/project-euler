import scala.collection.mutable

object Problem66 {

  lazy val answer: Int = dMaximizingX(maxD = 1000)

  def dMaximizingX(maxD: Int): Int = {

    val solutions = mutable.Map[Int, (Int, Int)]() // D -> (x, y)

    val unsolvedD = mutable.Set[Int]((
      (1 to maxD).toSet &~
        Stream.from(1).map(x => x*x).takeWhile(_ <= maxD).toSet
    ).toSeq :_*)

    val roots = (2 to maxD).map(d => (d, math.sqrt(d))).toMap

    var x = 2
    var xSquaredMinusOne = 3
    var sqrtXSquaredMinusOne = math.sqrt(xSquaredMinusOne)
    var xSquared = 4
    while (unsolvedD.nonEmpty) {
      for (d <- unsolvedD) {
        solveForY(x = x, d = d,
          xSquared = xSquared,
          sqrtXSquaredMinusOne = sqrtXSquaredMinusOne,
          sqrtD = roots(d)
        ) foreach { y =>
          solutions put (d, (x, y))
          unsolvedD -= d
          println(solutions.size + "       " + (d, (x, y)))
        }
      }
      xSquaredMinusOne = xSquared + x + x
      xSquared = xSquaredMinusOne + 1
      x += 1
      sqrtXSquaredMinusOne = math.sqrt(xSquaredMinusOne)
    }

    solutions.maxBy(_._2._1)._1
  }

  def solveForD(x: Int, y: Int): Option[Int] = {
    val d = ((x.toDouble*x - 1) / (y*y)).round.toInt
    if (x*x - d*y*y == 1) Some(d.toInt) else None
  }

  def solveForY(x: Int, d: Int): Option[Int] = {
    val y = math.sqrt((x.toDouble*x - 1) / d).round.toInt
    if (x*x - d*y*y == 1) Some(y.toInt) else None
  }

  def solveForY(x: Int, d: Int,
    xSquared: Int,
    sqrtXSquaredMinusOne: Double,
    sqrtD: Double
  ): Option[Int] = {
    val y = ( sqrtXSquaredMinusOne / sqrtD ).round.toInt
    if (xSquared - d*y*y == 1) Some(y.toInt) else None
  }

  case class Equation(x: Int, y: Int, d: Int) {
    def test = x*x - d*y*y == 1
  }
}
