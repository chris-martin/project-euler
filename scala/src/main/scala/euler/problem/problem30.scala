package euler.problem

package object problem30 {

  final case class NrOfDigits(n: Int) {
    def maxPowerSum: Int = n * BigInt(9).pow(5).toInt
    def minValue: Int = BigInt(10).pow(n-1).toInt
    def isFeasible: Boolean = maxPowerSum >= minValue
  }

  def isMagic(n: Int): Boolean =
    n == n.toString.map({ c =>
      BigInt(c.toString) pow 5
    }).sum

  def magicNrs = {

    val maxNrOfDigits =
      Stream.from(1)
        .map(NrOfDigits)
        .takeWhile(_.isFeasible)
        .last

    (2 to maxNrOfDigits.maxPowerSum).filter(isMagic)
  }

  def answer = magicNrs.sum

}
