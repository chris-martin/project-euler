package problems

import (
    "strconv"
    "github.com/chris-martin/project-euler/util/prime"
    "github.com/chris-martin/project-euler/project-euler/util/fibonacci"
)

const thousand = 1000
const million = thousand * thousand

func Answer(problemNumber int) string {
    switch problemNumber {

    case 1:
        a := 0
        for i := 1; i < 1000; i++ {
            if i % 3 == 0 || i % 5 == 0 {
                a += i
            }
        }
        return strconv.Itoa(a)

    case 2:
        c := make(chan int, 100)
        go fibonacci.Fibs(c)
        a := 0
        for f := range c {
            if f % 2 == 0 {
                a += f
            }
            if f >= 4 * million {
                return strconv.Itoa(a)
            }
        }

    case 3:
        return strconv.Itoa(prime.LargestPrimeFactor(600851475143))

    }
    return ""
}
