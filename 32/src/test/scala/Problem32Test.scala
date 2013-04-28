class Problem32Test extends org.scalatest.FreeSpec {

  "Products" in {
    info(
      Problem32.products.mkString(", ")
    )
  }

  "Answer is correct" in {
    expectResult(45228) {
      Problem32.answer
    }
  }

}
