import PrimeFunctions.factorize

object Problem5 {

  lazy val answer: Int =
    Bag.union((2 to 20).map(factorize(_))).toSeq.product
}
