object Main extends App {

  val products = Set((
    for (
      permutation <- (1 to 9).permutations;
      product <- productsForDigits(permutation)
    ) yield product
  ).toSeq: _*)

  def productsForDigits(digits: Seq[Int]): Seq[Int] =
    (
      for (
        a <- 1 until digits.size;
        b <- a+1 until digits.size
      ) yield {

        def slice(i: Int, j: Int): Int =
          digits.slice(i, j).mkString.toInt

        val x = slice(0, a)
        val y = slice(a, b)
        val z = slice(b, digits.size)

        if (x * y == z)
          Some(z)
        else
          None
      }
    ).flatten

  println("Products: %s".format(products.mkString(", ")))
  println("Sum: %s".format(products.sum))
}
