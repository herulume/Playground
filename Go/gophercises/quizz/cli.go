package quizz

import (
	"fmt"
	"io"
	"os"
)

type Cli struct {
	quizz Game
	out   io.Writer
}

func NewCLI(quizzLocation string) (*Cli, error) {
	quizz, err := NewQuizz(quizzLocation)

	if err != nil {
		return nil, fmt.Errorf("Error creating CLI for the quizz %v", err)
	}

	cli := Cli{quizz, os.Stdout}

	return &cli, nil
}

const loadingPrompt = "Loading default quizz..."

func (c *Cli) Play() {
	fmt.Fprintln(c.out, loadingPrompt)

	c.quizz.Start(c.out)

	c.quizz.Finish(c.out)
}
