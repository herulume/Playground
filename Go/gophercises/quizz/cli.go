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

const loadingPrompt = "Loading the game..."

func (c *Cli) Play() {
	fmt.Fprintln(c.out, loadingPrompt)

	c.game.Start(c.out, c.in)

	c.game.Finish(c.out)
}
