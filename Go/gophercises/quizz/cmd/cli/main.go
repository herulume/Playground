package main

import (
	"fmt"

	"github.com/herulume/quizz"
)

func main() {
	fmt.Println("Loading default quizz...")
	q, err := quizz.LoadDefaultQuizz()

	if err != nil {
		fmt.Printf("Failed to create the quizz %v", err)
	}

	fmt.Print(q)
}
