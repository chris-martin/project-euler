object Main extends App {

  println(answer)

  def answer: Int = {

    lazy val fibs: Stream[Int] = 0 #:: 1 #::
      fibs.zip(fibs.tail).map { n => n._1 + n._2 }

    fibs.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum
  }

}
