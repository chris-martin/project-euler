package euler.problem

object Problem11 {

  type Grid = Array[Array[Int]]

  lazy val grid: Grid =
    io.Source.fromFile("../problems/11-data.txt")
      .getLines.map(_.trim).filter(_.nonEmpty)
      .map(_.split(" ").map(_.toInt)).toArray

  def shifted(grid: Grid): Grid =
    grid.zipWithIndex.map({ case (row: Array[Int], i: Int) =>
      (Array.fill(i)(0) ++ row).padTo(2 * grid.length, 0)
    }).transpose[Int]

  lazy val answer: Int =
    Stream(
      grid,
      grid.transpose[Int],
      shifted(grid),
      shifted(grid.map(_.reverse))
    )
    .flatten.view.flatMap(_.sliding(4).map(_.product)).max

}
