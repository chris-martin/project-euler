import collection.mutable

object Main {
  def main(args: Array[String]) {

    def collatz(i: Long): Long =
      if (i % 2 == 0) (i / 2)
      else (3 * i + 1)

    val memo = mutable.HashMap[Long, Long](1L -> 1L)

    def calculate(i: Long) {
      if (!memo.contains(i)) {
        val stack = mutable.Stack[Long](i)
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

    println((1L to 1000000L).maxBy(length(_)))

  }
}
