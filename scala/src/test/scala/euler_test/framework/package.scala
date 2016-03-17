package euler_test

import java.time.{Duration, Instant}
import scalaz._, Scalaz._, effect._, IO._
import org.apache.commons.lang3.exception.ExceptionUtils.getStackTrace

package object framework {

  sealed trait Test

  final case class TestExecution(
    name: String,
    result: IO[TestResult]) extends Test

  final case class TestGroup(
    name: String,
    tests: IList[Test]) extends Test

  final case class TestResult(
    success: Boolean,
    text: String)

  def catchTestException(x: IO[TestResult]): IO[TestResult] =
    x.catchLeft.map {
      case -\/(e) => testFailure(getStackTrace(e))
      case \/-(result) => result
    }

  def testSuccess(text: String): TestResult =
    TestResult(success=true, text=text)

  def testFailure(text: String): TestResult =
    TestResult(success=false, text=text)

  final case class TestFailure(
    name: String,
    text: String)

  final case class SuiteState(
    runs: IORef[Int],
    failures: IORef[Vector[TestFailure]])

  def initSuiteState: IO[SuiteState] =
    for {
      runs <- newIORef[Int](0)
      failures <- newIORef[Vector[TestFailure]](Vector.empty)
    } yield SuiteState(runs, failures)

  /** @return An exit code (assuming this will be used to
    *         implement a `main` function).
    */
  def run(tests: IList[Test]): IO[Int] =
    for {
      state    <- initSuiteState
      _        <- tests.map(runTest(_, state, indent = 0)).sequence
      runs     <- state.runs.read
      failures <- state.failures.read
      _        <- putStrLn(s"\nTests run: $runs\nFailures: ${failures.length}")
      _        <- failures.map(f =>
                    putStrLn(s"  ${f.name}: ${f.text}")).sequence
    } yield if (failures.isEmpty) 0 else 1

  def runTest(test: Test, state: SuiteState, indent: Int): IO[Unit] =
    test match {
      case t: TestExecution => runTestExecution(t, state, indent)
      case t: TestGroup     => runTestGroup    (t, state, indent)
    }

  def runTestExecution(test: TestExecution, state: SuiteState,
                       indent: Int): IO[Unit] =
    for {
      _      <- putStr(s"${"  " * indent}${test.name}: ")
      start  <- IO { Instant.now() }
      result <- test.result |> catchTestException
      end    <- IO { Instant.now() }
      duration = Duration.between(start, end)
      _      <- putStrLn(s"${result.text} [${duration.toMillis} ms]")
      _      <- if (result.success) ().pure[IO] else
                  state.failures.mod(_ :+ TestFailure(test.name, result.text))
      _      <- state.runs.mod(_ + 1)
    } yield ()

  def runTestGroup(test: TestGroup, state: SuiteState,
                   indent: Int): IO[Unit] =
    for {
      _ <- putStrLn(test.name)
      _ <- test.tests.map(runTest(_, state, indent=indent+1)).sequence
    } yield ()

}
