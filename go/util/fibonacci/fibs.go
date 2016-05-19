package fibonacci

// Sends all of the fibonacci numbers to a channel.
//
// This is an unbounded sequence, so do not call this
// this an unbounded channel!
func Fibs(c chan int) {
    x, y := 1, 1
    for i := 0; true; i++ {
        c <- x
        x, y = y, x+y
    }
}
