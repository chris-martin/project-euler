object Problem14 extends App {

  println(answer)

  def answer: Int = {

    def collatz(i: Long): Long =
      if (i % 2 == 0) (i / 2)
      else (3 * i + 1)

    val memo = collection.mutable.HashMap[Long, Long](1L -> 1L)

    def calculate(i: Long) {
      if (!memo.contains(i)) {
        val stack = collection.mutable.Stack[Long](i)
        while (stack.nonEmpty) {
          val c = collatz(stack.head)
          if (memo.contains(c)) memo.put(stack.pop(), 1 + memo(c))
          else stack.push(c)
        }
      }
    }

    def length(i: Long): Long = {
      calculate(i)
      memo(i)
    }

    (1 to 1000000).maxBy(length(_))

  }

}
