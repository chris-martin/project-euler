object Main {
  def main(args: Array[String]) {

    println(Stream.from(2).filter(i => BigInt(i).isProbablePrime(30))(10000))

  }
}
