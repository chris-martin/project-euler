object Main {
  def main(args: Array[String]) {

    println((1 to 999).filter(i => (i % 3 == 0) || (i % 5 == 0)).sum)

  }
}
