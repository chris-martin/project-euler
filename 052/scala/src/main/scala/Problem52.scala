object Problem52 {

  case class DigitCounts(counts: List[Int])

  object DigitCounts {

    def apply(i: Int): DigitCounts = DigitCounts({
      val map = i.toString.toSeq.groupBy(x=>x).mapValues(_.size)
      ('0' to '9').map(digit => map.getOrElse(digit, 0)).toList
    })

  }

  implicit class RichIterable[A](it: Iterable[A]) {

    def allSame: Boolean = it.forall(_ == it.head)

  }

  lazy val answer: Int =
    Stream.from(1).filter(x => (1 to 6).map(i => DigitCounts(i*x)).allSame).head

}
