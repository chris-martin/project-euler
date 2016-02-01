package euler.problem

import Problem59._

class Test59 extends org.scalatest.FreeSpec {

  "Answer is correct" in assertResult(107359)(answer)

  "Decrypt with zeros" in assert("abc".decrypt("\0\0") === "abc")

  "Double decryption is the identity" in {
    val key = "j2xf"
    assert("abcdef".decrypt(key).decrypt(key) === "abcdef")
  }

  "Strings" in assert(
    strings('a' to 'c', 2)
    ===
    "aa"::"ab"::"ac"::"ba"::"bb"::"bc"::"ca"::"cb"::"cc"::Nil
  )

}
