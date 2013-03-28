object Main {
  def main(args: Array[String]) {

    implicit class Sq(x: Int) {
      def square: Int = x * x
    }

    println((1 to 100).sum.square - (1 to 100).map(_.square).sum)

  }
}
