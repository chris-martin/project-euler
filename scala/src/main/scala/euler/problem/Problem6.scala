package euler.problem

object Problem6 {

  lazy val answer: Int =
    (1 to 100).sum.square - (1 to 100).map(_.square).sum

  implicit class Sq(x: Int) {
    def square: Int = x * x
  }

}
