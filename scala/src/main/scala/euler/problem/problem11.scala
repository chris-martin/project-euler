package euler.problem

package object problem11 {

  type Grid = Array[Array[Int]]

  def shifted(grid: Grid): Grid =
    grid.zipWithIndex.map({ case (row: Array[Int], i: Int) =>
      (Array.fill(i)(0) ++ row).padTo(2 * grid.length, 0)
    }).transpose[Int]

  def answer(input: String): Int = {

    val grid: Grid =
      input.split("\n").map(_.trim).filter(_.nonEmpty)
        .map(_.split(" ").map(_.toInt))

    Stream(
      grid,
      grid.transpose[Int],
      shifted(grid),
      shifted(grid.map(_.reverse))
    )
    .flatten.view.flatMap(_.sliding(4).map(_.product)).max
  }

}
