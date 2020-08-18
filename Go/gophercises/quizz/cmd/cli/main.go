package main

import (
	"flag"
	"fmt"

	"github.com/herulume/quizz"
)

func main() {
	csv := flag.String("csv", "problems.csv", "a csv file in the format 'question,answer'")
	flag.Parse()

	q, err := quizz.NewCLI(*csv, os.Stdout, os.Stdin)

	if err != nil {
		fmt.Printf("Failed to create the quizz %v", err)
	}

	q.Play()
}
