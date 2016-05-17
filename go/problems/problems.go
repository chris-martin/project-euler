package problems

import (
    "strconv"
)

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

    }
    return ""
}
