object Problem18 {

  println(answer)

  lazy val rows: Array[Array[Int]] = io.Source
    .fromURL(getClass.getResource("triangle.txt"))
    .getLines.map(_.trim).filter(_.nonEmpty)
    .map(_.split(' ').map(_.toInt)).toArray

  // position (row, col) -> utility of that node
  lazy val utility = {

    val u = new collection.mutable.HashMap[(Int, Int), Int] {
      // the triangle has implicit 0 values below it
      override def default(key: (Int, Int)) =
        if (key._1 >= rows.size) 0 else super.default(key)
    }

    for (
      row <- (0 until rows.size).reverse;
      col <- (0 to row)
    ) {
      u.put(
        (row, col),
        rows(row)(col) + Seq(
          u(row+1, col),
          u(row+1, col+1)
        ).max
      )
    }

    u
  }

  def answer: Int = utility((0, 0))

}
