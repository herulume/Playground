package main

import (
	"flag"
	"fmt"
	"os"
	"time"

	"github.com/herulume/quizz"
)

func main() {
	csv := flag.String("csv", "problems.csv", "a csv file in the format 'question,answer'")
	timeLimit := flag.Int("limit", 30, "the time limit for the quizz in seconds")
	flag.Parse()

	time := time.Duration(*timeLimit) * time.Second

	game, err := quizz.NewQuizz(*csv, time)

	if err != nil {
		fmt.Printf("Failed to create the quizz %v", err)
                os.Exit(1)
	}

	q := quizz.NewCLI(game, os.Stdout, os.Stdin)

	q.Play()
}
