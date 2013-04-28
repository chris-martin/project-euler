object Problem25 extends App {

  println(answer)

  lazy val fibs: Stream[BigInt] = 0 #:: 1 #:: (
    (fibs zip fibs.tail) map { n => n._1 + n._2 }
  )

  def answer: Int =
    fibs.indexWhere(_.toString.size >= 1000)

}
