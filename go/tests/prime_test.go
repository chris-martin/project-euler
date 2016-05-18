package tests

import (
    "fmt"
    "github.com/chris-martin/project-euler/util/prime"
    "testing"
)

func ExampleLargestPrimeFactor() {
    fmt.Printf("%d %d %d %d\n",
        prime.LargestPrimeFactor(2),
        prime.LargestPrimeFactor(3),
        prime.LargestPrimeFactor(4),
        prime.LargestPrimeFactor(99))
    // Output: 2 3 2 11
}

//-- prop> forAll (elements $ take 50 primes) (\p -> largestPrimeFactor p == p)

func TestLargestPrimeFactor(t *testing.T) {
    for i := 2; i < 200; i++ {
        if prime.Test(i) {
            if prime.LargestPrimeFactor(i) != i {
                t.Logf("LargestPrimeFactor(%d)", i)
                t.Fail()
            }
        }
    }
}
