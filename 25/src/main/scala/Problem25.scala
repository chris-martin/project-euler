object Problem25 {

  lazy val fibs: Stream[BigInt] = 0 #:: 1 #:: (
    (fibs zip fibs.tail) map { n => n._1 + n._2 }
  )

  lazy val answer: Int =
    fibs.indexWhere(_.toString.size >= 1000)

}
