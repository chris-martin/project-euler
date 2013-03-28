object Main {
  def main(args: Array[String]) {

    println((1 to 100).map(BigInt(_)).product.toString.map(_.toString.toInt).sum)

  }
}
