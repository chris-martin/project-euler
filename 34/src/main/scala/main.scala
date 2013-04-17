object Main extends App {

  lazy val factorials: Stream[BigInt] =
    BigInt(1) #:: Stream.from(1).zip(factorials).map(x => x._2 * x._1)

  val maxDigits = Stream.from(1).takeWhile({ i =>
    (factorials(9) * i) >= (BigInt(10) pow i)
  }).last

  def curious(n: Int): Boolean =
    n == n.toString.map(_.toString.toInt).map(factorials(_)).sum

  println(
    (3 to BigInt(10).pow(maxDigits).toInt).filter(curious(_)).sum
  )

}
