object Main {
  def main(args: Array[String]) {

    println(('0' to '9').permutations.drop(999999).next().mkString)

  }
}
