object Main {
  def main(args: Array[String]) {

    println(BigInt(2).pow(1000).toString(10).map(_.toString.toInt).sum)

  }
}
