package main

import (
	"flag"
	"fmt"

	"github.com/herulume/quizz"
)

func main() {
	csv := flag.String("file", "problems.csv", "File to load the quizz from")
	flag.Parse()

	q, err := quizz.NewCLI(*csv)

	if err != nil {
		fmt.Printf("Failed to create the quizz %v", err)
	}

	q.Play()
}
