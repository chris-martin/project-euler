object Problem5 extends App {

  println(answer)

  def answer: Int = (
    Stream from 1
      filter { a =>
        (3 to 20) forall {
          b => a % b == 0
        }
      }
    ).head

}
