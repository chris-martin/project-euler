object Problem36 extends App {

  println(answer)

  def answer: Int =
    (
      (1 to 999999)
        map(BigInt(_))
        filter { x =>
          pal(x.toString(10)) && pal(x.toString(2))
        }
    ).sum.toInt

  def pal(s: String): Boolean =
    s == s.reverse

}
