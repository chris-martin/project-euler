https://projecteuler.net/problem=54

In the card game poker, a hand consists of five cards and are ranked, from
lowest to highest, in the following way:

* High Card: Highest value card.
* One Pair: Two cards of the same value.
* Two Pairs: Two different pairs.
* Three of a Kind: Three cards of the same value.
* Straight: All cards are consecutive values.
* Flush: All cards of the same suit.
* Full House: Three of a kind and a pair.
* Four of a Kind: Four cards of the same value.
* Straight Flush: All cards are consecutive values of same suit.
* Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest
value wins; for example, a pair of eights beats a pair of fives (see example 1
below). But if two ranks tie, for example, both players have a pair of queens,
then highest cards in each hand are compared (see example 4 below); if the
highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:

<table>
  <tr>
    <th>Hand</th>
    <th>Player 1</th>
    <th>Player 2</th>
    <th>Winner</th>
  </tr>
  <tr>
    <th>1</th>
    <td>5H 5C 6S 7S KD<br/>Pair of Fives</td>
    <td>2C 3S 8S 8D TD<br/>Pair of Eights</td>
    <td>Player 2</td>
  </tr>
  <tr>
    <th>2</th>
    <td>5D 8C 9S JS AC<br/>Highest card Ace</td>
    <td>2C 5C 7D 8S QH<br/>Highest card Queen</td>
    <td>Player 1</td>
  </tr>
  <tr>
    <th>3</th>
    <td>2D 9C AS AH AC<br/>Three Aces</td>
    <td>3D 6D 7D TD QD<br/>Flush with Diamonds</td>
    <td>Player 2</td>
  </tr>
  <tr>
    <th>4</th>
    <td>4D 6S 9H QH QC<br/>Pair of Queens<br/>Highest card Nine</td>
    <td>3D 6D 7H QD QS<br/>Pair of Queens<br/>Highest card Seven</td>
    <td>Player 1</td>
  </tr>
  <tr>
    <th>5</th>
    <td>2H 2D 4C 4D 4S<br/>Full House<br/>With Three Fours</td>
    <td>3C 3D 3S 9S 9D<br/>Full House<br/>with Three Threes</td>
    <td>Player 1</td>
  </tr>
</table>

The file, poker.txt, contains one-thousand random hands dealt to two players.
Each line of the file contains ten cards (separated by a single space): the
first five are Player 1's cards and the last five are Player 2's cards. You can
assume that all hands are valid (no invalid characters or repeated cards), each
player's hand is in no specific order, and in each hand there is a clear
winner.

How many hands does Player 1 win?
