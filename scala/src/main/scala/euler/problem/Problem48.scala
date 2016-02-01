package euler.problem

object Problem48 {

  lazy val answer = (1 to 1000).map(i => BigInt(i) pow i)
    .sum.toString.takeRight(10).mkString.toLong

}
