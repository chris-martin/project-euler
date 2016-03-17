package euler.problem

import java.io.Reader

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

package object problem54 {

  def answer: Int = {
    val games: Seq[Game] = PokerParser.parseFile(
      Source.fromFile("../problems/54-data.txt").reader())
    games.count(_.winner == 0)
  }

  trait CardAttribute {
    def toChar: Char
    def toInt: Int
  }

  trait CardAttributeCompanion[A <: CardAttribute] {
    protected def constructFromInt(i: Int): A
    def characters: String
    val values: Seq[A] = (0 until characters.length) map constructFromInt
    val fromChar: Map[Char, A] = values.map(x => (x.toChar, x)).toMap
    def apply(c: Char): A = fromChar(c)
    implicit val ordering: Ordering[A] = Ordering.by(_.toInt)
    def fromInt(i: Int): A = fromChar(characters(i))
  }

  class Ordinal private (val toInt: Int) extends CardAttribute {
    val toChar: Char = Ordinal.characters(toInt)
    override def toString = s"Ordinal($toChar)"
    def to(that: Ordinal): Seq[Ordinal] = (toInt to that.toInt).map(Ordinal.fromInt)
  }
  implicit object Ordinal extends CardAttributeCompanion[Ordinal] {
    lazy val characters = "23456789TJQKA"
    protected def constructFromInt(i: Int) = new Ordinal(i)
  }

  class Suit private (val toInt: Int) extends CardAttribute {
    val toChar: Char = Suit.characters(toInt)
    override def toString: String = s"Suit($toChar)"
  }
  implicit object Suit extends CardAttributeCompanion[Suit] {
    lazy val characters = "HCSD"
    protected def constructFromInt(i: Int) = new Suit(i)
  }

  final case class Card(ordinal: Ordinal, suit: Suit)

  final case class Hand(cards: Set[Card]) {

    def sortedOrdinals = cards.toSeq.map(_.ordinal).sorted

    def high: Ordinal = sortedOrdinals.max
    def low: Ordinal = sortedOrdinals.min

    def isStraight: Boolean = sortedOrdinals == (low to high)

    def isFlush: Boolean = cards.toSeq.map(_.suit).distinct.size == 1

    def isStraightFlush: Boolean = isStraight && isFlush

    def nOfAKind(n: Int): Set[Ordinal] =
      cards.groupBy(_.ordinal).filter(_._2.size == n).keySet

    def threeOfAKind: Option[Ordinal] = nOfAKind(3).headOption

    def fourOfAKind: Option[Ordinal] = nOfAKind(4).headOption

    def fullHouse: Option[(Ordinal, Ordinal)] =
      nOfAKind(3).headOption.flatMap(a => nOfAKind(2).headOption.map(b => (a, b)))

    def twoPairs: Option[(Ordinal, Ordinal)] =
      nOfAKind(2).toSeq.sorted.reverse.toList match {
        case a :: b :: Nil => Some((a, b))
        case _ => None
      }

    def onePair: Option[Ordinal] = nOfAKind(2).toList match {
      case a :: Nil => Some(a)
      case _ => None
    }

  }

  object Hand {
    def parse(s: String): Hand = PokerParser.parseAll(PokerParser.hand, s).get
  }

  object StraightFlush {
    def unapply(hand: Hand): Option[Ordinal] =
      if (hand.isStraightFlush) Some(hand.high) else None
  }

  object FourOfAKind {
    def unapply(hand: Hand): Option[Ordinal] = hand.fourOfAKind
  }

  object FullHouse {
    def unapply(hand: Hand): Option[(Ordinal, Ordinal)] = hand.fullHouse
  }

  object Flush {
    def unapply(hand: Hand): Option[Unit] = if (hand.isFlush) Some() else None
  }

  object Straight {
    def unapply(hand: Hand): Option[Ordinal] = if (hand.isStraight) Some(hand.high) else None
  }

  object ThreeOfAKind {
    def unapply(hand: Hand): Option[Ordinal] = hand.threeOfAKind
  }

  object TwoPairs {
    def unapply(hand: Hand): Option[(Ordinal, Ordinal)] = hand.twoPairs
  }

  object OnePair {
    def unapply(hand: Hand): Option[Ordinal] = hand.onePair
  }

  implicit object intSeqOrdering extends Ordering[Seq[Int]] {
    def compare(x: Seq[Int], y: Seq[Int]) = {
      val a = x.zipAll(y, -1, -1).dropWhile(x => x._1 == x._2).head
      val result = a._1 - a._2
      assert(result != 0)
      result
    }
  }

  implicit val handRanking: Ordering[Hand] = Ordering.by[Hand, Seq[Int]]({ hand: Hand =>
    val w = hand match {
      case StraightFlush(a) => Seq(9, a.toInt)
      case FourOfAKind(a)   => Seq(8, a.toInt)
      case FullHouse(a, b)  => Seq(7, a.toInt, b.toInt)
      case Flush(a)         => Seq(6)
      case Straight(a)      => Seq(5, a.toInt)
      case ThreeOfAKind(a)  => Seq(4, a.toInt)
      case TwoPairs(a, b)   => Seq(3, a.toInt, b.toInt)
      case OnePair(a)       => Seq(2, a.toInt)
      case _                => Seq(1)
    }
    w ++ hand.sortedOrdinals.reverse.map(_.toInt)
  })

  final case class Game(hands: Seq[Hand]) {
    def winner: Int = hands.zipWithIndex.maxBy(_._1)._2
  }

  object Game {
    def parse(handStrings: String*): Game =
      new Game(handStrings.map(Hand.parse))
  }

  object PokerParser extends RegexParsers {

    def cardAttributeParser[A <: CardAttribute](implicit companion: CardAttributeCompanion[A]): Parser[A] =
      s"[${companion.characters}]".r ^^ { s => companion.apply(s.head) }

    lazy val ordinal: Parser[Ordinal] = cardAttributeParser[Ordinal]
    lazy val suit: Parser[Suit] = cardAttributeParser[Suit]
    lazy val card: Parser[Card] = ordinal ~ suit ^^ { x => Card(x._1, x._2) }
    lazy val hand: Parser[Hand] = repN(5, card <~ opt(' ')) ^^ { cards => Hand(cards.toSet) }
    lazy val game: Parser[Game] = repN(2, hand <~ opt(' ')) ^^ { hands => Game(hands) }
    lazy val file: Parser[Seq[Game]] = phrase(rep(game <~ opt("""\r?\n""".r)))

    def parseFile(s: Reader): Seq[Game] = parse(file, s).get

  }

}
