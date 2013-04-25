object Main extends App {

  val triangles = Stream.from(1).map(n => n*(n+1)/2)

  println(
    io.Source.fromURL(getClass.getResource("words.txt"))
      .mkString.replace("\"", "").split(",")
      .map(_.toUpperCase.map(_-'A'+1).sum)
      .count(v => triangles.takeWhile(_ <= v).contains(v))
  )
}
