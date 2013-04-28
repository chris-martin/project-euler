object Problem40 {

  lazy val d: Stream[Int] = Stream.from(1)
    .flatMap(_.toString.map(_.toString.toInt))

  lazy val answer = (0 to 6)
    .map(i => d(BigInt(10).pow(i).toInt - 1))
    .product

}
