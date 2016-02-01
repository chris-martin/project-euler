package euler.problem

object Problem29 {

  lazy val answer: Int =
    (
      for { a <- 2 to 100
            b <- 2 to 100 }
      yield
        BigInt(a) pow b

    ).toSet.size

}
