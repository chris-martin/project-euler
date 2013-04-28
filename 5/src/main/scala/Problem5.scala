object Problem5 {

  lazy val answer: Int = (
    Stream from 1
      filter { a =>
        (3 to 20) forall {
          b => a % b == 0
        }
      }
    ).head

}
