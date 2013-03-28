object Main {
  def main(args: Array[String]) {

    val memo = collection.mutable.HashMap[(Int, Int), Long]()

    // how many n-bit strings have weight w?
    def count(n: Int, w: Int): Long = {
      if (w < 0 || w > n) 0
      else if (w == 0 || w == n) 1
      else if (memo.contains((n, w))) memo((n, w))
      else {
        val c = count(n-1, w) + count(n-1, w-1)
        memo.put((n, w), c)
        c
      }
    }

    println(count(40, 20))

  }
}
