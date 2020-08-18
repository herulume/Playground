package quizz_test

import (
	"bytes"
	"testing"
	"time"

	"github.com/herulume/quizz"
)

var wantedQuizz = []quizz.Round{
	{"1+1", "2"},
	{"2*2", "4"},
	{"Obj(Cat)", "Small categories"},
}

func TestNewQuizz(t *testing.T) {

	t.Run("it loads a quizz from a file", func(t *testing.T) {
		quizzFile := "test.csv"
		_, closeF := quizz.CreateTempFile(t, quizzFile, wantedQuizz)
		defer closeF()

		got, err := quizz.NewQuizz(quizzFile, 30*time.Second)

		quizz.AssertNoError(t, err)
		quizz.AssertQuizz(t, got.Quizz, wantedQuizz)
	})

}

func TestQuizzStart(t *testing.T) {
	quizzFile := "test.csv"
	_, closeF := quizz.CreateTempFile(t, quizzFile, wantedQuizz)
	defer closeF()

	t.Run("it prints all the questions", func(t *testing.T) {
		QuestionsSpy := bytes.Buffer{}
		fakeStdin := bytes.Buffer{}
		fakeStdin.Write([]byte("\n\n\n"))

		q, _ := quizz.NewQuizz(quizzFile, 30*time.Second)

		q.Start(&QuestionsSpy, &fakeStdin)

		quizz.AssertQuestions(t, QuestionsSpy, wantedQuizz)
	})

	t.Run("it sets points accordingly to our answers", func(t *testing.T) {
		discard := bytes.Buffer{}
		fakeStdin := bytes.Buffer{}
		fakeStdin.Write([]byte("2\n4\n1\n"))

		q, _ := quizz.NewQuizz(quizzFile, 30*time.Second)

		q.Start(&discard, &fakeStdin)

		got := q.Points
		want := uint(2)

		if got != want {
			t.Errorf("expected %v points, got %v", want, got)
		}
	})
}
