package quizz_test

import (
	"bytes"
	"reflect"
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
		assertQuizz(t, got.Quizz, wantedQuizz)
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

		assertQuestions(t, QuestionsSpy, wantedQuizz)
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

	t.Run("it scores points before timing out", func(t *testing.T) {
		discard := bytes.Buffer{}
		fakeStdin := bytes.Buffer{}
		fakeStdin.Write([]byte("2\n"))

		q, _ := quizz.NewQuizz(quizzFile, 1*time.Second)

		q.Start(&discard, &fakeStdin)
		expectedPointsBeforeTimeOut := uint(1)

		if q.Points != expectedPointsBeforeTimeOut {
			t.Errorf("expected %d points before timing out, got %d", expectedPointsBeforeTimeOut, q.Points)
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

func assertQuestions(t *testing.T, gotB bytes.Buffer, quizz []quizz.Round) {
	t.Helper()

	var sb strings.Builder
	for i, q := range quizz {
		s := q.FormatQuestion(i)
		sb.WriteString(s)
	}

	got := gotB.String()
	want := sb.String()

	if got != want {
		t.Errorf("got %v, want %v", got, want)
	}

}

func assertQuizz(t *testing.T, got, want []quizz.Round) {
	t.Helper()

	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %v, want %v", got, want)
	}
}
