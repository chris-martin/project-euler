class Problem17Test extends org.scalatest.FreeSpec {

  "Strings" in {
    Problem17.strings
      .grouped(100)
      .map(_.mkString(" "))
      .foreach(info(_))
  }

  "Answer is correct" in {
    expectResult(21124) {
      Problem17.answer
    }
  }

}
