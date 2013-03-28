object Main {
  def main(args: Array[String]) {

    println(Stream.range(2, 2000000).map(BigInt(_)).filter(_.isProbablePrime(30)).sum)

  }
}
