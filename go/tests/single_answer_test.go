package tests

import (
    "testing"
    "os"
    "strconv"
)

func TestSingleAnswer(t *testing.T) {

    s := os.Getenv("PROBLEM")

    switch {

        case s == "":
            t.Log("No problem specified.")

        default:
            i, err := strconv.Atoi(s)
            if err != nil {
                panic(err)
            }
            TestAnswer(t, i)

    }
}
