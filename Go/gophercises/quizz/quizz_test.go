package quizz_test

import (
	"bytes"
	"strings"
	"testing"
	"time"

	"github.com/herulume/quizz"
)

const tmpFileName = "test.csv"
const quizzDuration30s = 30 * time.Second

var wantedQuizz = []quizz.Round{
	{"1+1", "2"},
	{"2*2", "4"},
	{"Obj(Cat)", "Small categories"},
}

func TestNewQuizz(t *testing.T) {

	t.Run("it loads a quizz from a file", func(t *testing.T) {
		quizzFile := tmpFileName
		_, closeF := quizz.CreateTempFile(t, quizzFile, wantedQuizz)
		defer closeF()

		got, err := quizz.NewQuizz(quizzFile, 30*time.Second)

		quizz.AssertNoError(t, err)
		quizz.AssertQuizz(t, got.Quizz, wantedQuizz)
	})

}

func TestQuizzStart(t *testing.T) {
	quizzFile := tmpFileName
	_, closeF := quizz.CreateTempFile(t, quizzFile, wantedQuizz)
	defer closeF()

	t.Run("it prints all the questions", func(t *testing.T) {
		QuestionsSpy := bytes.Buffer{}
		fakeStdin := bytes.Buffer{}
		fakeStdin.Write([]byte("\n\n\n"))

		q, _ := quizz.NewQuizz(quizzFile, quizzDuration30s)

		q.Start(&QuestionsSpy, &fakeStdin)

		quizz.AssertQuestions(t, QuestionsSpy, wantedQuizz)
	})

	t.Run("it sets points accordingly to answers", func(t *testing.T) {
		discard := bytes.Buffer{}
		fakeStdin := bytes.Buffer{}
		fakeStdin.Write([]byte("2\n4\n1\n"))

		q, _ := quizz.NewQuizz(quizzFile, quizzDuration30s)

		q.Start(&discard, &fakeStdin)

		got := q.Points
		want := uint(2)

		if got != want {
			t.Errorf("expected %v points, got %v", want, got)
		}
	})

	t.Run("it times out, warning the user", func(t *testing.T) {
		discard := bytes.Buffer{}
		TimeOutSpy := bytes.Buffer{}

		q, _ := quizz.NewQuizz(quizzFile, 1*time.Nanosecond)

		q.Start(&TimeOutSpy, &discard)
		got := TimeOutSpy.String()

		if !strings.Contains(got, quizz.QuizzTimedOut) {
			t.Error("expected a timeout message, didn't get any")
		}
	})
}

func TestQuizzFinish(t *testing.T) {
	quizzFile := tmpFileName
	_, closeF := quizz.CreateTempFile(t, quizzFile, wantedQuizz)
	defer closeF()

	FinishSpy := bytes.Buffer{}
	q, _ := quizz.NewQuizz(quizzFile, quizzDuration30s)
	q.Finish(&FinishSpy)

	want := quizz.QuizzEnd + q.FormatFinalPoints()
	got := FinishSpy.String()

	if got != want {
		t.Errorf("expected %v, got %v", got, want)
	}
}
