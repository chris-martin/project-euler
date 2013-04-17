object Main {
  def main(args: Array[String]) {

    case class NrOfDigits(n: Int) {
      def maxPowerSum: Int = n * BigInt(9).pow(5).toInt
      def minValue: Int = BigInt(10).pow(n-1).toInt
      def isFeasible: Boolean = maxPowerSum >= minValue
    }

    val maxNrOfDigits = Stream.from(1).map(NrOfDigits(_)).takeWhile(_.isFeasible).last
    def isMagic(n: Int): Boolean = n == n.toString.map({ c => BigInt(c.toString) pow 5 }).sum
    val magicNrs = (2 to maxNrOfDigits.maxPowerSum).filter(isMagic(_))

    Seq(
      "Numbers: %s".format(magicNrs.mkString(", ")),
      "Sum: %s".format(magicNrs.sum)
    ).foreach(println(_))

  }
}
