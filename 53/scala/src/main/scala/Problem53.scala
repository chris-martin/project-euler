object Problem53 {

  implicit class RichInt(n: Int) {
    def factorial: BigInt = (2 to n).foldLeft(BigInt(1))(_*_)
    def choose(r: Int): BigInt = n.factorial / ( r.factorial * (n-r).factorial )
  }

  lazy val answer: Int = (1 to 100).flatMap(n => (1 to n).map(r => n choose r)).count(_ > 1000000)

}
