package euler.util

package object palindrome {

  def palindromeStr(s: String): Boolean =
    s == s.reverse

  def palindromeInt(i: BigInt, base: Int = 10): Boolean =
    palindromeStr(i.toString(base))

}
