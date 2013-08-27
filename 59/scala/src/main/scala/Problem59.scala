object Problem59 {

  lazy val cipher: String = io.Source.fromFile("../cipher1.txt")
    .getLines().mkString.split(',').map(_.toInt.toChar).mkString

  implicit class RichString(s: String){

    def xor(t: String): String =
      (s zip t).map({ case (a,b) => (a^b).toChar }).mkString

    def decrypt(key: String): String =
      s.grouped(key.length).map(_ xor key).mkString

  }

  def strings(characters: Iterable[Char], length: Int): Iterable[String] =
    (1 to length).foldLeft[Iterable[String]]("" :: Nil) {
      (it, _) => characters.flatMap(c => it.map(c +))
    }

  lazy val goodCharacters: Set[Char] =
    (('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') ++ " ,.:;()-!?").toSet

  lazy val answer: Int =
    strings('a' to 'z', 3).map(cipher decrypt).toSeq
      .maxBy(_ count (goodCharacters contains))
      .map(_.toInt).sum

}
