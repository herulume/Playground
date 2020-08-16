package main

import (
	"fmt"

	"github.com/herulume/quizz"
)

func main() {
	q, err := quizz.NewCLI("problems.csv")

	if err != nil {
		fmt.Printf("Failed to create the quizz %v", err)
	}

	q.Play()
}
