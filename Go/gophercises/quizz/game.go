package quizz

import "io"

// Game manages the state of a game.
type Game interface {
	Start(alertsDestination io.Writer)
	Finish(alertDestination io.Writer)
}
