object Problem43 {

  lazy val answer: Long =
    (0 to 9).permutations.map(_.mkString)
      .filter({ s =>
        s.substring(1, 4).toInt % 2 == 0 &&
        s.substring(2, 5).toInt % 3 == 0 &&
        s.substring(3, 6).toInt % 5 == 0 &&
        s.substring(4, 7).toInt % 7 == 0 &&
        s.substring(5, 8).toInt % 11 == 0 &&
        s.substring(6, 9).toInt % 13 == 0 &&
        s.substring(7, 10).toInt % 17 == 0
      })
      .map(_.toLong)
      .sum

}
