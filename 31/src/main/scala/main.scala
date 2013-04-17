object Main {

  val denominations = Seq(1, 2, 5, 10, 20, 50, 100, 200)

  val target = 200

  implicit class Purse(quantities: Seq[Int] = Nil) {
    def pence: Int = (denominations zip quantities).map(x => x._1 * x._2).sum
  }

  def count(base: Seq[Int] = Nil): Int = {

    val p = base.pence

    if (p == target)
      1
    else if (p > target)
      0
    else if (base.size == denominations.size)
      0
    else
      (0 to (target-p)/denominations(base.size)).map({ n => count(base :+ n) }).sum
  }

  def main(args: Array[String]) {
    println(count())
  }

}
