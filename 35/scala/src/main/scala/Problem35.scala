object Problem35 {

  lazy val answer: Int =
    (2 to 999999) count { x =>
      val s = x.toString
      (0 to s.size) forall { i =>
        val t = s.substring(i) + s.substring(0, i)
        BigInt(t).isProbablePrime(40)
      }
    }

}