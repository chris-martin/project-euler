package prime

// The largest prime p such that p divides n.
func LargestPrimeFactor(n int) int {
    for {
        if Test(n) {
            return n
        }
        n /= SmallestPrimeFactor(n)
    }
}

// The smallest prime p such that p divides n.
func SmallestPrimeFactor(n int) int {
    i := 2
    for {
        if n % i == 0 {
            return i
        }
        i++
    }
}
