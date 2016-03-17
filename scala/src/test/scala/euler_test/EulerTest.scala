package euler_test

import scalaz._, Scalaz._, effect._

object EulerTest extends SafeApp {

  override def runl(args: List[String]): IO[Unit] =
    args match {

      case Nil =>
        framework.run(IList(problem.tests)).map(System.exit)

      case List(n) =>
        framework.run(IList(problem.answerTest(n.toInt))).map(System.exit)
    }

}
