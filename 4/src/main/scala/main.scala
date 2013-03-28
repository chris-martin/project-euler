object Main {
  def main(args: Array[String]) {

    val candidates = for (i <- 1 to 999; j <- i to 999) yield i * j
    val isPalindrome = {x: String => x == x.reverse}
    println(candidates.filter(x => isPalindrome(x.toString)).max)

  }
}
