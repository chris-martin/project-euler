package euler_test

import framework._
import scalaz._, Scalaz._, effect._, Maybe._

package object problem {

  /** The tests for Euler problems. It includes checks for answers for problems
    * that have suitably fast answers, and tests for problems' modules.
    */
  def tests: Test = TestGroup("Problems with fast answers",
    IList(1, 2, 3, 5, 6, 7, 8, 9, 11, 13, 15, 16, 17, 18, 19, 20,
        22, 28, 29, 33, 40, 42
       ).map(answerTest))

  /** A test case that checks the answer for Euler problem n against the known
    * answers in answers.txt at the root of the repository. */
  def answerTest(n: Int): Test =
    TestExecution(
      s"Problem $n",
      for {
        correctAnswerMaybe <- getCorrectAnswer(n)
        gotAnswerMaybe     <- euler.problem.answer(n)
      } yield (correctAnswerMaybe, gotAnswerMaybe) match {

        case (Just(correctAnswer), Just(gotAnswer)) =>
          if (gotAnswer == correctAnswer) testSuccess(correctAnswer)
          else testFailure(s"Got $gotAnswer, but the correct answer is $correctAnswer.")

        case (Just(correctAnswer), Empty()) => testFailure(
          s"Answer is not defined. The correct answer is $correctAnswer.")

        case (Empty(), Just(gotAnswer)) => testFailure(
          s"Got $gotAnswer, but I don't know if it's correct or not.")

        case _ => testFailure("I've never heard of that problem.")
      }
    )

    def iteratorToIList[A](it: Iterator[A]): IList[A] =
      it.foldRight[IList[A]](INil())(ICons(_, _))

    /** Read the known answer for Euler problem n
      * from answers.txt at the root of the repository, or None if the
      * file doesn't contain the answer for that problem. */
    def getCorrectAnswer(n: Int): IO[Maybe[String]] = for {
      lines <- IO { val source = scala.io.Source.fromFile("../answers.txt")
                    try iteratorToIList(source.getLines) finally source.close() }
    } yield lines
      .map(_.trim.split("\\s+") match { case Array(a, b) => (a, b) })
      .filter(_._1 == n.toString).map(_._2).headMaybe

}
