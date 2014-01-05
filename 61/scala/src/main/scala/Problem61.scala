object Problem61 {

  lazy val answer: Int = sixCycle.sum

  lazy val sixCycle: Seq[Int] = cycles(groups)(0).map(_.toInt)

  def cycles(groups: Seq[Seq[String]]): Seq[Seq[String]] =
    groups.permutations.flatMap({ permutation =>
      chains(Nil, permutation).filter(chain => connected(chain.last, chain.head))
    }).toSeq

  def chains(chain: Seq[String], groups: Seq[Seq[String]]): Seq[Seq[String]] =
    chain match {
      case Nil =>
        groups.head
          .flatMap(x => chains(Seq(x), groups.tail))
      case _ =>
        groups match {
          case Nil => Seq(chain)
          case group :: moreGroups =>
            group
              .filter(connected(chain.last, _))
              .flatMap(x => chains(chain :+ x, moreGroups))
        }
    }

  def connected(x: String, y: String) =
    x.substring(2).equals(y.substring(0, 2))

  lazy val groups = List[Int => Int](
    n => n*(n+1)/2,
    n => n*n,
    n => n*(3*n-1)/2,
    n => n*(2*n-1),
    n => n*(5*n-3)/2,
    n => n*(3*n-2)
  ).map({ f =>
    Iterator.iterate(1)(_+1)
      .map(f)
      .map(_.toString)
      .dropWhile(_.length < 4)
      .takeWhile(_.length == 4)
      .toSeq
  })
}
