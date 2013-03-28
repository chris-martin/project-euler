object Main {
  def main(args: Array[String]) {

    val fibs = {
      var x = List(0, 1)
      Stream.continually { x = List(x(1), x.sum); x(1) }
    }

    println(fibs.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum)

  }
}
