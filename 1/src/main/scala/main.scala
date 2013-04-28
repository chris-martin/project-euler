object Main extends App {

  println(answer)

  def answer: Int =
    (1 to 999).filter(i => (i % 3 == 0) || (i % 5 == 0)).sum

}
