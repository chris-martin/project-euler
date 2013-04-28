object Main extends App {
  println(
    (1 to 9)
      .flatMap(i => (1 to i).permutations)
      .map(_.mkString).sorted.reverse
      .filter(BigInt(_).isProbablePrime(40))
      .head
  )
}