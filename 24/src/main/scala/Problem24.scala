object Problem24 {

  def answer: String =
    ('0' to '9').permutations.drop(999999).next().mkString

}
