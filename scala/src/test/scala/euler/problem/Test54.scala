package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem54._

class Test54 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(5000, Millis)

  "example 1" in assertResult(1) {
    Game.parse("5H 5C 6S 7S KD", "2C 3S 8S 8D TD").winner
  }

  "example 2" in assertResult(0) {
    Game.parse("5D 8C 9S JS AC", "2C 5C 7D 8S QH").winner
  }

  "example 3" in assertResult(1) {
    Game.parse("2D 9C AS AH AC", "3D 6D 7D TD QD").winner
  }

  "example 4" in assertResult(0) {
    Game.parse("4D 6S 9H QH QC", "3D 6D 7H QD QS").winner
  }

  "example 5" in assertResult(0) {
    Game.parse("2H 2D 4C 4D 4S", "3C 3D 3S 9S 9D").winner
  }

  "4 of a kind" in assertResult(Some(Ordinal('9'))) {
    Hand.parse("9H 2D 9D 9C 9S").fourOfAKind
  }

  "3 of a kind" in assertResult(Some(Ordinal('9'))) {
    Hand.parse("9H 2D 5D 9C 9S").threeOfAKind
  }

  "3 of a kind (too many)" in assertResult(None) {
    Hand.parse("9H 2D 9D 9C 9S").threeOfAKind
  }

  "3 of a kind (too few)" in assertResult(None) {
    Hand.parse("9H 2D AD 8C 9S").threeOfAKind
  }

  "flush" in assertResult(true) {
    Hand.parse("9H 2H AH 8H JH").isFlush
  }

  "flush (not)" in assertResult(false) {
    Hand.parse("9H 2D AH 8H JH").isFlush
  }

  "low" in assertResult(Ordinal('7')) {
    Hand.parse("9H TD 8H JH 7S").low
  }

  "high" in assertResult(Ordinal('J')) {
    Hand.parse("9H TD 8H JH 7S").high
  }

  "ordinal range" in assertResult(List(Ordinal('6'), Ordinal('7'), Ordinal('8'))) {
    (Ordinal('6') to Ordinal('8')).toList
  }

  "straight" in assertResult(true) {
    Hand.parse("9H TD 8H JH 7S").isStraight
  }

  "straight (not)" in assertResult(false) {
    Hand.parse("9H TD 8H JH 6S").isStraight
  }

  "parse card" in assert(
    PokerParser.parseAll(PokerParser.card, "5D").get
    ===
    Card(Ordinal('5'), Suit('D'))
  )

  "parse hand" in assert(
    PokerParser.parseAll(PokerParser.hand, "JS 7S 5C KD 6D").get
    ===
    Hand(Set(
      Card(Ordinal('J'), Suit('S')),
      Card(Ordinal('7'), Suit('S')),
      Card(Ordinal('5'), Suit('C')),
      Card(Ordinal('K'), Suit('D')),
      Card(Ordinal('6'), Suit('D'))
    ))
  )

  "parse game" in assert(
    PokerParser.parseAll(PokerParser.game, "TS QH TD QS 3C JH AH 2C 8D 7D").get
    ===
    Game(Seq(
      Hand(Set(
        Card(Ordinal('T'), Suit('S')),
        Card(Ordinal('Q'), Suit('H')),
        Card(Ordinal('T'), Suit('D')),
        Card(Ordinal('Q'), Suit('S')),
        Card(Ordinal('3'), Suit('C'))
      )),
      Hand(Set(
        Card(Ordinal('J'), Suit('H')),
        Card(Ordinal('A'), Suit('H')),
        Card(Ordinal('2'), Suit('C')),
        Card(Ordinal('8'), Suit('D')),
        Card(Ordinal('7'), Suit('D'))
      ))
    ))
  )

  "parse file" in assert(
    PokerParser.parseAll(PokerParser.file,
      """JH 5C TD 4C 6H JS KD KH QS 4H
        |TC KH JC 4D 9H 9D 8D KC 3C 8H
        |2H TC 8S AD 9S 4H TS 7H 2C 5C""".stripMargin
    ).get.size === 3
  )

}
