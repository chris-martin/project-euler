object Problem39 extends App {

  println(answer)

  def answer: Int = {
    val maxPerimeter = 1000L
    (
      for (
        a <- (1L to maxPerimeter);
        b <- (1L to maxPerimeter)
      ) yield (a, b, sqrt(a*a + b*b))
    ).flatMap({
      case (a,b,c) =>
        c.map(a + b + _)
    }).filter(_ <= maxPerimeter)
    .groupBy(identity)
    .maxBy(_._2.size)
    ._1.toInt
  }

  def sqrt(i: Long): Option[Long] = {
    val x = math.sqrt(i).round
    if (x*x == i) Some(x) else None
  }

}
