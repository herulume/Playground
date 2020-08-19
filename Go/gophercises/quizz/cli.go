package quizz

import (
	"fmt"
	"io"
)

type Cli struct {
	game Game
	out  io.Writer
	in   io.Reader
}

func NewCLI(game Game, out io.Writer, in io.Reader) *Cli {
	return &Cli{
		game: game,
		out:  out,
		in:   in,
	}
}

const LoadingPrompt = "Loading the game...\n"

func (c *Cli) Play() {
	fmt.Fprintf(c.out, LoadingPrompt)

	c.game.Start(c.out, c.in)

	c.game.Finish(c.out)
}
