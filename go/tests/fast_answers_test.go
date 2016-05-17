package tests

import (
    "testing"
)

func TestFastAnswers(t *testing.T) {
    for _, i := range []int{1, 2} {
        TestAnswer(t, i)
    }
}
