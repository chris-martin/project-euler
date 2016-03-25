package euler.util

/** English-language representations of integers,
  * used by Euler problem 17. */
package object number_words {

  def word(n: Int): String = n match {
    case  1 => "one"
    case  2 => "two"
    case  3 => "three"
    case  4 => "four"
    case  5 => "five"
    case  6 => "six"
    case  7 => "seven"
    case  8 => "eight"
    case  9 => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 14 => "fourteen"
    case 15 => "fifteen"
    case 16 => "sixteen"
    case 17 => "seventeen"
    case 18 => "eighteen"
    case 19 => "nineteen"
    case 20 => "twenty"
    case 30 => "thirty"
    case 40 => "forty"
    case 50 => "fifty"
    case 60 => "sixty"
    case 70 => "seventy"
    case 80 => "eighty"
    case 90 => "ninety"

    case i if i < 100 =>
      val a = i / 10
      val b = i % 10
      word (10 * a) ++ word(b)

    case i if i < 1000 =>
      val a = i / 100
      val b = i % 100
      if (b == 0)
        word(a) ++ "hundred"
      else
        word(a) ++ "hundredand" ++ word(b)

    case 1000 => "onethousand"
  }

}
