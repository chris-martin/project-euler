object Main {

  def main(args: Array[String]) {

    println(1 + (1 to 500).flatMap(i => {
      val j = 2*i
      val x = square(j+1)
      Seq(x, x-j, x-2*j, x-3*j)
    }).sum)

  }

  def square(n: Int): Int = n*n

}
