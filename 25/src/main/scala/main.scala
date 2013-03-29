object Main {
  def main(args: Array[String]) {

    // from the documentation of scala.collection.Stream
    val fib = {
     def loop(h: BigInt, n: BigInt): Stream[BigInt] = h #:: loop(n, h + n)
     loop(1, 1)
    }

    println(fib.zip(Stream.from(1)).find(n => n._1.toString.size >= 1000).get._2)

  }
}
