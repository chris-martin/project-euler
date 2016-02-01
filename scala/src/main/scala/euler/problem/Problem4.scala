package euler.problem

object Problem4 extends App {

  lazy val answer: Int =
    (1 to 999).combinations(2)
      .map(x => x(0) * x(1))
      .filter(_.isPalindrome)
      .max

  implicit class RichInt(i: Int) {

    def isPalindrome: Boolean = {
      val s = i.toString
      s == s.reverse
    }

  }

}
