class Problem59Test extends org.scalatest.FreeSpec {

  import Problem59._

  "Answer is correct" in {
    assert ( answer === 107359 )
  }

  "Decrypt with zeros" in {
    assert ( "abc".decrypt("\0\0") === "abc" )
  }

  "Double decryption is the identity" in {
    val key = "j2xf"
    assert ( "abcdef".decrypt(key).decrypt(key) === "abcdef" )
  }

  "Strings" in {
    assert (
      strings('a' to 'c', 2)
      ===
      "aa"::"ab"::"ac"::"ba"::"bb"::"bc"::"ca"::"cb"::"cc"::Nil
    )
  }

}
