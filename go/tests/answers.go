package tests

import (
    "bufio"
    "os"
    "regexp"
    "strconv"
    "testing"
    "github.com/chris-martin/project-euler/problems"
)

func TestAnswer(t *testing.T, i int) {

    correctAnswers := GetCorrectAnswers()

    got := problems.Answer(i)
    expected := correctAnswers[strconv.Itoa(i)]
    switch {

        case expected == "":
            t.Logf("Problem %d: Don't know the correct answer. Got \"%s\"", i, got)
            t.Fail()

        case got != expected:
            t.Logf("Problem %d: Expected \"%s\", got \"%s\"", i, expected, got)
            t.Fail()

        default:
            t.Logf("Problem %d: %s", i, got)

    }
}

func GetCorrectAnswers () map[string]string {
    answers := make(map[string]string)
    r := regexp.MustCompile(`^\s*([0-9]+)\s+([^\s]+)$`)

    file, err := os.Open(os.Getenv("EULER_PATH") + "/answers.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        line := scanner.Text()
        m := r.FindStringSubmatch(line)
        if m != nil {
            answers[m[1]] = m[2]
        }
    }

    if err := scanner.Err(); err != nil {
        panic(err)
    }

    return answers
}
