object Main {
  def main(args: Array[String]) {

    println(Set((for (a <- (2 to 100); b <- 2 to 100) yield BigInt(a) pow b): _*).size)

  }
}
