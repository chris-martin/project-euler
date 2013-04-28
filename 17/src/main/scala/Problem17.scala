object Problem17 {

  lazy val answer: Int = strings.map(_.size).sum

  lazy val strings: Seq[String] = {

    val lows = """one two three four five six seven eight nine ten
      eleven twelve thirteen fourteen fifteen sixteen seventeen
      eighteen nineteen""".trim.split("""\s+""").map(_.capitalize)
    assert(lows.size == 19)

    val oneDigit = lows.take(9)
    assert(oneDigit.size == 9)

    val tens = "twenty thirty forty fifty sixty seventy eighty ninety".
      split(' ').map(_.capitalize)
    assert(tens.size == 8)

    val oneAndTwoDigit = lows ++ (
      for (a <- tens; b <- Seq("") ++ lows.take(9))
      yield (a + b)
    )
    assert(oneAndTwoDigit.size == 99)

    val threeDigit = (
      for (a <- oneDigit; b <- Seq("") ++ oneAndTwoDigit.map("And" + _))
      yield (a + "Hundred" + b)
    )
    assert(threeDigit.size == 900)

    val all = oneAndTwoDigit ++ threeDigit :+ "OneThousand"
    assert(all.size == 1000)
    all
  }

}
