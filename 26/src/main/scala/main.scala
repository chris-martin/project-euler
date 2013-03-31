object Main {

  def main(args: Array[String]) {
    println((2 to 999).maxBy(oneOver(_).cycle.size))
  }

  def oneOver(n: Int): Result = State(n).finalState.result

  case class Result(base: Seq[Int], cycle: Seq[Int]) {
    def asString(repetitions: Int): String = base.mkString + Stream.fill(repetitions)(cycle.mkString).mkString
  }
  object Result {
    def apply(x: (Seq[Int], Seq[Int])): Result = Result(x._1, x._2)
    def apply(base: String, cycle: String): Result = Result(base.map(_.toString.toInt), cycle.map(_.toString.toInt))
  }

  case class State(n: Int, previousState: Option[State] = None) {
    lazy val previousStates: Stream[State] = (previousState #:: previousStates.map(_.previousState)).takeWhile(_.isDefined).map(_.get)
    lazy val previousDigits: Seq[Int] = previousState map (_.digits) getOrElse Nil
    lazy val lastDigit: Int = ( ( (BigInt(10) pow (previousDigits.size + 1)) - (BigInt(10) * previousInteger * n) ) / n ).toInt
    lazy val digits: Seq[Int] = previousDigits :+ lastDigit
    lazy val nextState: State = copy(previousState = Some(this))
    lazy val isFinal: Boolean = digits contains nextState.lastDigit
    lazy val previousInteger: BigInt = if (previousDigits.isEmpty) 0 else BigInt(previousDigits.mkString)
    lazy val finalState: State = { var x: State = this; while (!x.isFinal) x = x.nextState; x }
    lazy val result: Result = Result(digits.splitAt(digits lastIndexOf nextState.lastDigit))
  }

}
