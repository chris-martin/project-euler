object Main {
  def main(args: Array[String]) {

    var x = BigInt(600851475143L)
    while (!x.isProbablePrime(30)) {
      x /= Stream.from(2).find(i => (x mod i) == 0).get
    }
    println(x)

  }
}
