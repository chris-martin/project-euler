package euler.problem

import euler.util.fibonacci.fibs

object Problem25 {

  lazy val answer: Int =
    fibs.indexWhere(_.toString.length >= 1000)

}
