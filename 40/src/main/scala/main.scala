object Main extends App {

  def d: Stream[Int] = Stream.from(1).flatMap(_.toString.map(_.toString.toInt))

  println((0 to 6).map(i => d(BigInt(10).pow(i).toInt - 1)).product)

}
