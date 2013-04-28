object Problem29 extends App {

  println(answer)

  def answer: Int =
    Set(
      (

        for (
          a <- 2 to 100;
          b <- 2 to 100
        )
        yield
          BigInt(a) pow b

      ): _*
    ).size

}
