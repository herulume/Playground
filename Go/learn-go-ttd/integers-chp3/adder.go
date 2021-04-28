package integers

import "fmt"

// Add takes two integers and returns the sum of them.
func Add(x, y int) int {
	return x + y
}

func ExampleAdd() {
    sum := Add(1, 5)
    fmt.Println(sum)
    // Output: 6
}
