import org.scalatest.FreeSpec

class Problem44Test extends FreeSpec {

  (1 to 100).map({ p =>
    p.toString in {
      expectResult(Problem44.stream.contains(p)) {
        Problem44.isPentagonal(p)
      }
    }
  })

}
