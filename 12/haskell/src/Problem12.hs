module Problem12 where

--import Data.Numbers.Primes (primes)

answer :: Integer
answer = 5
--triangles.drop(1).filter(nrOfFactors(_) > 500).head.toInt

triangles :: [Integer]
triangles = scanl (+) 1 [2..]

factorCount :: Integer -> Integer
factorcount n = foldl' step (n, [])
  where step (n, factors)
    | n == 1 = factors

    lowestPrimeFactor primes n = find (`divides` n) primes

def nrOfFactors(n: BigInt): Int = {
    val fs = new collection.mutable.HashMap[BigInt, Int]
    var i = n
    while (i != 1) {
      val p = primes.find(p => i % p == 0).get
      fs.put(p, fs.getOrElse(p, 0) + 1)
      i /= p
    }
    fs.values.map(1 + _).product
  }

divides :: Integer -> Integer -> Bool
n `divides` d = d `mod` n == 0
