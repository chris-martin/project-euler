package problems

import (
    "strconv"
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
            a := 0
            i := 1
            j := 1
            for {
                f := i + j
                i = j
                j = f
                if f % 2 == 0 {
                    a += f
                }
                if f >= 4 * million {
                    return strconv.Itoa(a)
                }
            }

    }
    return ""
}
