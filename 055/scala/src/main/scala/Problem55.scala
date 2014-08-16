object Problem55 {

  lazy val answer: Int =
    (0 until 10000) count { n =>

      lazy val iterations: Stream[BigInt] = BigInt(n) #::
        ( iterations map { i => i + BigInt(i.toString.reverse) } )

      !(
        iterations drop 1 take 49 exists { i =>
          val s = i.toString
          s == s.reverse
        }
      )

    }

}
