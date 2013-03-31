import scala.math.BigDecimal, BigDecimal.RoundingMode
import org.scalatest._

import Main._

class Test extends FunSuite {

  test("1/2")  { expectResult(Result("5",   "0"))      (oneOver(2))  }
  test("1/3")  { expectResult(Result("",    "3"))      (oneOver(3))  }
  test("1/4")  { expectResult(Result("25",  "0"))      (oneOver(4))  }
  test("1/5")  { expectResult(Result("2",   "0"))      (oneOver(5))  }
  test("1/6")  { expectResult(Result("1",   "6"))      (oneOver(6))  }
  test("1/7")  { expectResult(Result("",    "142857")) (oneOver(7))  }
  test("1/8")  { expectResult(Result("125", "0"))      (oneOver(8))  }
  test("1/9")  { expectResult(Result("",    "1"))      (oneOver(9))  }
  test("1/10") { expectResult(Result("1",   "0"))      (oneOver(10)) }

  (11 to 50) foreach { n =>
    test("1/%d".format(n)) {
      val result = oneOver(n).asString(repetitions = 3)
      val correct = BigDecimal(1d/n).setScale(result.size, RoundingMode.DOWN)
      expectResult(correct.toString)("0." + result)
    }
  }

}
