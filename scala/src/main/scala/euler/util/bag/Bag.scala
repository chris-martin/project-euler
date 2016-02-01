package euler.util.bag

trait Bag[A] {

  /** How many elem are in the bag? */
  def cardinalityOf(elem: A): Int

  /** Add one elem to the bag */
  def :+(elem: A): Bag[A]

  def toSeq: Seq[A]

  def distinct: Set[A]
}

object Bag {

  def empty[A]: Bag[A] = new Bag[A] {
    override def cardinalityOf(elem: A) = 0
    override def :+(elem: A) = fromMap(Map(elem -> 1))
    override def toSeq = Seq.empty
    override def distinct = Set.empty
  }

  private def fromMap[A](map: Map[A, Int]): Bag[A] = new Bag[A] {
    override def cardinalityOf(elem: A) = map.getOrElse(elem, 0)
    override def :+(elem: A) =
      fromMap(map.updated(elem, 1 + cardinalityOf(elem)))
    override def toSeq =
      map.flatMap({
        case (elem, cardinality) => Seq.fill(cardinality)(elem)
      }).toSeq
    override def distinct = map.keySet
  }

  /** Union of any number of bags */
  def union[A](bags: Seq[Bag[A]]): Bag[A] =
    fromMap(Map[A, Int]((for {
      elem <- bags.map(_.distinct).foldLeft[Set[A]](Set.empty)(_ union _).toSeq
      cardinality = bags.map(_.cardinalityOf(elem)).max
    } yield (elem: A, cardinality: Int)): _*))
}
