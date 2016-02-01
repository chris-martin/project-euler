package euler.problem

object Problem1 {

  lazy val answer: Int =
    (1 to 999)
      .filter(i => (i % 3 == 0) || (i % 5 == 0))
      .sum

}
