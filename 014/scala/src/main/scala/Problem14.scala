import scala.annotation.tailrec

object Problem14 {

  def collatz(i: Long): Long =
    if (i % 2 == 0) i / 2
    else 3 * i + 1

  type Lengths = Map[Long, Int]

  @tailrec
  def getLengths(lengths: Lengths, stack: List[Long]): Lengths =
    stack match {

      // The stack is empty, we're done!
      case Nil => lengths

      // The next item on the stack is already calculated,
      case knownValue :: newStack if lengths contains knownValue =>
        // so just pop it off.
        getLengths(lengths, newStack)

      // We're looking at the tree edge x -> y, where length(x) is unknown
      case x :: restOfStack =>
        val y = collatz(x)

        // See if we already know length(y)
        lengths.get(y) match {

          // length(y) is already known,
          case Some(yLength) =>
            // so length(x) is now known to be length(y) + 1
            getLengths(lengths.updated(x, yLength + 1), restOfStack)

          // We haven't learned anything except that there's a new
          // value y that we need to know;
          case None =>
            // push y onto the stack.
            getLengths(lengths, y +: stack)
        }
    }

  def keyWithMaxValue[A, B : Ordering](map: Map[A, B]): A =
    map.maxBy(_._2)._1

  val initialLengths = Map(1L -> 1)

  val initialStack = (1 to 1000000).map(_.toLong).toList

  lazy val answer: Long =
    keyWithMaxValue(getLengths(initialLengths, initialStack))
}
