package prime

import "math/big"

func Test(n int) bool {
    b := big.NewInt(int64(n))
    return b.ProbablyPrime(20)
}
