(ns Problem3.core)

(def upperBound 4e6)

(defn bounded [xs]
  (take-while (fn [x] (< x upperBound)) xs))

(def fibs
  (concat '(0 1) (lazy-seq (map + fibs (rest fibs)))))

(defn answer []
  (reduce + (filter even? (bounded fibs))))









  lazy val answer: Int = {

    val primes = Stream.from(0).map(BigInt(_))
      .filter(_.isProbablePrime(10)).map(_.toInt)

    var x = BigInt("600851475143")
    while (!x.isProbablePrime(40)) {
      x /= primes.filter(i => (x mod i) == 0).head
    }

    x.toInt
  }
