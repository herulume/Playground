package quizz

import (
	"fmt"
	"io"
	"time"
)

type Cli struct {
	game Game
	out  io.Writer
	in   io.Reader
}

func NewCLI(gameLocation string, out io.Writer, in io.Reader, timer time.Duration) (*Cli, error) {
	game, err := NewQuizz(gameLocation, timer)

	if err != nil {
		return nil, fmt.Errorf("Error creating CLI for the game %v", err)
	}

	cli := Cli{game, out, in}

	return &cli, nil
}

const loadingPrompt = "Loading the game..."

func (c *Cli) Play() {
	fmt.Fprintln(c.out, loadingPrompt)

	c.game.Start(c.out, c.in)

	c.game.Finish(c.out)
}
