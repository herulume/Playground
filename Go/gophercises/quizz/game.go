package quizz

import "io"

// Game manages the state of a game.
type Game interface {
	Start(alertsdestination io.Writer, input io.Reader)
	Finish(alertDestination io.Writer)
}
