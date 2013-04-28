object Problem9 extends App {

  println(answer)

  def answer: Int =
    (
      for (
        a <- (0 to 1000).toStream;
        b <- ((a + 1) to 1000).toStream
      ) yield {

        val c = 1000 - (a + b)

        if (b < c && a*a + b*b == c*c)
          Some(Seq(a, b, c))
        else
          None
      }
    ).view.flatten.head.product

}
