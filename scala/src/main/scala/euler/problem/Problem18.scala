package euler.problem

import java.lang.Math.max

object Problem18 {

  lazy val rows: Seq[Seq[Int]] = io.Source
    .fromFile("../problems/18-data.txt")
    .getLines.map(_.trim).filter(_.nonEmpty)
    .map(_.split(' ').map(_.toInt).toSeq).toSeq

  def zip3[A, B, C](as: Seq[A], bs: Seq[B], cs: Seq[C]): Seq[(A, B, C)] =
    as zip bs zip cs map { case ((a, b), c) => (a, b, c) }

  def reduceRow(xs: Seq[Int], ys: Seq[Int]): Seq[Int] =
    zip3(xs, ys, ys.drop(1)).map({ case (x, y1, y2) => x + max(y1, y2) })

  def reduceTriangle(rows: Seq[Seq[Int]]): Seq[Seq[Int]] =
    rows.dropRight(2) :+ reduceRow(rows.dropRight(1).last, rows.last)

  lazy val answer: Int =
    Stream.iterate(rows)(reduceTriangle).find(_.size == 1).get.head.head
}
