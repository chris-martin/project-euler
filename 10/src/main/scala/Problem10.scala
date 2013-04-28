object Problem10 extends App {

  println(answer)

  def answer: BigInt =
    Stream.range(2, 2000000)
      .map(BigInt(_))
      .filter(_.isProbablePrime(30))
      .sum

}
