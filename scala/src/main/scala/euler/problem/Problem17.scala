package euler.problem

object Problem17 {

  lazy val answer: Int = strings.map(_.length).sum

  lazy val strings: Seq[String] = {

    val `1-9` = Seq("one", "two", "three", "four", "five",
                    "six", "seven", "eight", "nine")

    val `10-19` = Seq("ten", "eleven", "twelve", "thirteen",
                      "fourteen", "fifteen", "sixteen", "seventeen",
                      "eighteen", "nineteen")

    val `20-99` =
      for {
        a <- Seq("twenty", "thirty", "forty", "fifty",
                 "sixty", "seventy", "eighty", "ninety")
        b <- "" +: `1-9`
      } yield a + b

    val `1-99` = `1-9` ++ `10-19` ++ `20-99`

    val `100-999` =
      for {
        a <- `1-9`
        b <- "" +: `1-99`.map("and" + _)
      } yield a + "hundred" + b

    val `1-1000` = `1-99` ++ `100-999` :+ "onethousand"

    assert(`1-1000`.size == 1000)

    `1-1000`
  }

}
