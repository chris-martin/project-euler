package euler.problem

package object problem19 {

  def answer: Int = {

    val dates = for {
      year <- 1900 to 2000
      month <- 1 to 12
      day <- 1 to daysInMonth(month, year)
    } yield Date(year, month, day)

    (dates zip Stream.continually(1 to 7).flatten)
      .count { x =>
        x._1.year != 1900 && x._1.day == 1 && x._2 == 7
      }
  }

  def isLeapYear(year: Int): Boolean =
    year match {
      case x if x % 400 == 0 => true
      case x if x % 100 == 0 => false
      case x if x %   4 == 0 => true
      case _                 => false
    }

  def daysInMonth(month: Int, year: Int): Int =
    month match {
      /* jan */ case  1 => 31
      /* feb */ case  2 => if (isLeapYear(year)) 29 else 28
      /* mar */ case  3 => 31
      /* apr */ case  4 => 30
      /* may */ case  5 => 31
      /* jun */ case  6 => 30
      /* jul */ case  7 => 31
      /* aug */ case  8 => 31
      /* sep */ case  9 => 30
      /* oct */ case 10 => 31
      /* nov */ case 11 => 30
      /* dec */ case 12 => 31
    }

  final case class Date(year: Int, month: Int, day: Int)

}
