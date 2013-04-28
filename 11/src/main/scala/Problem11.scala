object Problem11 extends App {

  println(answer)

  type Grid = Array[Array[Int]]

  lazy val grid: Grid =
    io.Source.fromURL(getClass.getResource("grid.txt"))
      .getLines.map(_.trim).filter(_.nonEmpty)
      .map(_.split(" ").map(_.toInt)).toArray

  def shifted(grid: Grid): Grid =
    grid.zipWithIndex.map({ case (row: Array[Int], i: Int) =>
      (Array.fill(i)(0) ++ row).padTo(2 * grid.size, 0)
    }).transpose[Int]

  def answer:Int =
    Stream(
      grid,
      grid.transpose[Int],
      shifted(grid),
      shifted(grid.map(_.reverse))
    )
      .flatten.view.flatMap(_.sliding(4).map(_.product)).max

}
