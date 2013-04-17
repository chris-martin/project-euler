object Main extends App {

  def catProduct(k: BigInt, n: Int): BigInt =
    BigInt((1 to n).map(_ * k).mkString)

  assert(catProduct(192, 3) == 192384576)

  def pan9(x: BigInt): Boolean = {
    val s = x.toString()
    s.size == 9 && (1 to 9).forall({ i => s.contains(i.toString) })
  }

  println(
    (1 to 99999).flatMap({ k =>
      Stream.from(2).
        map({ n => catProduct(k, n) }).
        takeWhile(_ <= 999999999)
    }).filter(pan9(_)).max
  )

}
