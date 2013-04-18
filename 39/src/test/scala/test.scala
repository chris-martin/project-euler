import org.scalatest.FreeSpec

class Test extends FreeSpec {

  import Main._

  val q = 61274L

  "root of a square" in {
    expectResult(Some(q)) {
      sqrt(q*q)
    }
  }

  "root of a non-square" in {
    expectResult(None) {
      sqrt(q*q + 1)
    }
  }

}
