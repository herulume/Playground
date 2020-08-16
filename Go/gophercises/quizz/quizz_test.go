package quizz_test

import (
	"bytes"
	"encoding/csv"
	"os"
	"reflect"
	"strings"
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
		_, closeF := createTempFile(t, quizzFile, wantedQuizz)
		defer closeF()

		got, err := quizz.NewQuizz(quizzFile)

		assertNoError(t, err)
		assertQuizz(t, got, &wantedQuizz)
	})
}

func TestQuizzStart(t *testing.T) {

	t.Run("it prints all the questions", func(t *testing.T) {
		QuestionsSpy := bytes.Buffer{}
		wantedQuizz.Start(&QuestionsSpy)

		asserQuestions(t, QuestionsSpy, &wantedQuizz)
	})
}

func asserQuestions(t *testing.T, gotB bytes.Buffer, quizzW *quizz.Quizz) {
	t.Helper()

	var sb strings.Builder
	for i, q := range quizzW.Quizz {
		s := q.FormatQuestion(i + 1)
		sb.WriteString(s)
	}

	got := gotB.String()
	want := sb.String()

	if got != want {
		t.Errorf("got %v, want %v", got, want)
	}

}

func assertQuizz(t *testing.T, got, want *quizz.Quizz) {
	t.Helper()

	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %v, want %v", got, want)
	}
}

func createTempFile(t *testing.T, filename string, quizz quizz.Quizz) (*os.File, func()) {
	t.Helper()

	tmpfile, err := os.Create(filename)

	if err != nil {
		t.Fatalf("could not create temp file %v", err)
	}

	csvwriter := csv.NewWriter(tmpfile)
	for _, round := range quizz.Quizz {
		var row []string
		row = append(row, round.Question, round.Answer)
		csvwriter.Write(row)
	}
	csvwriter.Flush()

	removeFile := func() {
		tmpfile.Close()
		os.Remove(tmpfile.Name())
	}

	return tmpfile, removeFile
}

func assertNoError(t *testing.T, err error) {
	t.Helper()

	if err != nil {
		t.Fatalf("didn't expect an error but got one, %v", err)
	}
}
