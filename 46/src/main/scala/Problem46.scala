object Problem46 {

  lazy val answer: Int = {

    val queue = collection.mutable.PriorityQueue[Node](
      Base()
    )(
      Ordering.by(_.value)
    ).reverse

    var highest = 1

    var a: Option[Int] = None

    while (a.isEmpty) {
      val node = queue.dequeue()
      queue ++= node.next
      val v = node.value
      if (v > highest) {
          a = (highest+1 until v).find(isOddComposite(_))
          highest = v
      }
    }

    a.get
  }

  def isOddComposite(n: Int): Boolean =
    (n % 2 == 1) && !BigInt(n).isProbablePrime(40)

  trait Cons[A] {
    def head: A
    def tail: Cons[A]
  }

  case class SquareDoubles(i: Int) extends Cons[Int] {
    def head = 2*i*i
    def tail = SquareDoubles(i+1)
  }

  implicit class StreamCons[A](stream: Stream[A]) extends Cons[A] {
    def head = stream.head
    def tail = stream.tail
  }

  sealed trait Node {
    def value: Int
    def next: List[Node]
  }

  case class Base(vertical: Cons[Int], horizontal: Cons[Int]) extends Node {

    lazy val value = vertical.head + horizontal.head

    def next = List(
      Stalk(vertical.tail, horizontal.head),
      Base(vertical, horizontal.tail)
    )

  }

  object Base {

    def apply(): Base = {
      Base(
        SquareDoubles(1),
        Stream.from(2).filter(BigInt(_).isProbablePrime(40))
      )
    }

  }

  case class Stalk(vertical: Cons[Int], horizontal: Int) extends Node {

    lazy val value = vertical.head + horizontal

    def next = List(
      Stalk(vertical.tail, horizontal)
    )

  }

}
