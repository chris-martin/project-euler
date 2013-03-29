import io.Source

object Main {
  def main(args: Array[String]) {

    println(
      Source.fromInputStream(getClass.getResourceAsStream("names.txt")).
        mkString.toUpperCase.split(",").map(_.trim.stripPrefix("\"").stripSuffix("\"")).sorted.
        map(name => name.map(_ - 'A' + 1).sum).zip(Stream.from(1)).map(x => x._1 * x._2).sum
    )

  }
}
