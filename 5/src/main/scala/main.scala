object Main {
  def main(args: Array[String]) {

    println(Stream.from(1).filter(a => (3 to 20).forall(b => a % b == 0)).head)

  }
}
