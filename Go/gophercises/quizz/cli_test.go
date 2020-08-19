package quizz_test

import (
	"bytes"
	"io"
	"testing"

	"github.com/herulume/quizz"
)

type GameSpy struct {
	StartCalled  bool
	FinishCalled bool
}

func (g *GameSpy) Start(alertsdestination io.Writer, input io.Reader) {
	g.StartCalled = true
}

func (g *GameSpy) Finish(alertDestination io.Writer) {
	g.FinishCalled = true
}

func TestCLI(t *testing.T) {

	t.Run("start and finish a game", func(t *testing.T) {
		game := &GameSpy{}

		out := &bytes.Buffer{}
		in := &bytes.Buffer{}

		quizz.NewCLI(game, out, in).Play()

		assertMessagesSentToUser(t, out, quizz.LoadingPrompt)
		assertGameStarted(t, game)
		assertGameFinished(t, game)
	})

}

func assertGameFinished(t *testing.T, game *GameSpy) {
	t.Helper()
	if !game.FinishCalled {
		t.Errorf("game should have finished")
	}
}

func assertGameStarted(t *testing.T, game *GameSpy) {
	t.Helper()
	if !game.StartCalled {
		t.Errorf("game should have started")
	}
}

func assertMessagesSentToUser(t *testing.T, stdout *bytes.Buffer, message string) {
	t.Helper()
	got := stdout.String()
	if got != message {
		t.Errorf("got %q sent to stdout but expected %+v", got, message)
	}
}
