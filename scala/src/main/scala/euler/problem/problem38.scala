package euler.problem

package object problem38 {

  def catProduct(k: BigInt, n: Int): BigInt =
    BigInt((1 to n).map(_ * k).mkString)

  def pan9(x: BigInt): Boolean = {
    val s = x.toString()
    s.length == 9 && (1 to 9).forall({ i => s.contains(i.toString) })
  }

  def answer =
    (1 to 99999)
      .flatMap({ k =>
        Stream.from(2).
          map({ n => catProduct(k, n) }).
          takeWhile(_ <= 999999999)
      })
      .filter(pan9)
      .max

}
