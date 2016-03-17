package euler.problem

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import problem59._

class Test59 extends org.scalatest.FreeSpec
with TimeLimitedTests {

  val timeLimit = Span(2000, Millis)

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
