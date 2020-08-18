package quizz_test

import (
	"bytes"
	"os"
	"testing"

	"github.com/herulume/quizz"
)

var wantedQuizz = quizz.Quizz{
	Quizz: []quizz.Round{
		{"1+1", "2"},
		{"2*2", "4"},
		{"Obj(Cat)", "Small categories"},
	},
	Points: 0,
}

func TestNewQuizz(t *testing.T) {

	t.Run("it loads a quizz from a file", func(t *testing.T) {
		quizzFile := "test.csv"
		_, closeF := quizz.CreateTempFile(t, quizzFile, wantedQuizz)
		defer closeF()

		got, err := quizz.NewQuizz(quizzFile)

		quizz.AssertNoError(t, err)
		quizz.AssertQuizz(t, got, &wantedQuizz)
	})

}

func TestQuizzStart(t *testing.T) {

	t.Run("it prints all the questions", func(t *testing.T) {
		QuestionsSpy := bytes.Buffer{}
		wantedQuizz.Start(&QuestionsSpy, os.Stdin)

		quizz.AssertQuestions(t, QuestionsSpy, &wantedQuizz)
	})

	t.Run("it sets points accordingly to our answers", func(t *testing.T) {
		discard := bytes.Buffer{}
		fakeStdin := bytes.Buffer{}
		fakeStdin.Write([]byte("2\n4\n1\n"))

		wantedQuizz.Points = 0

		wantedQuizz.Start(&discard, &fakeStdin)

		got := wantedQuizz.Points
		want := uint(2)

		if got != want {
			t.Errorf("expected %v points, got %v", want, got)
		}

		wantedQuizz.Points = 0
	})
}
