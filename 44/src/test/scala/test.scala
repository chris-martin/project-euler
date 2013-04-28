import org.scalatest.FreeSpec

class Test extends FreeSpec {

  import Main._

  (1 to 100).map({ p =>
    p.toString in {
      expectResult(stream.contains(p)) {
        isPentagonal(p)
      }
    }
  })

}
