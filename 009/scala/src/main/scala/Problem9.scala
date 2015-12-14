object Problem9 {

  def square(n: Int) = n * n

  lazy val answer: Int =
    (for {
      a <- 0 to 1000
      b <- 0 to a
      c = 1000 - (a + b)
      if square(a) + square(b) == square(c)
    } yield a * b * c).head

}
