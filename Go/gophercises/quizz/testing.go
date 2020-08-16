package quizz

import (
	"bytes"
	"encoding/csv"
	"os"
	"reflect"
	"strings"
	"testing"
)

func AssertQuestions(t *testing.T, gotB bytes.Buffer, quizzW *Quizz) {
	t.Helper()

	var sb strings.Builder
	for i, q := range quizzW.Quizz {
		s := q.FormatQuestion(i)
		sb.WriteString(s)
	}

	got := gotB.String()
	want := sb.String()

	if got != want {
		t.Errorf("got %v, want %v", got, want)
	}

}

func AssertQuizz(t *testing.T, got, want *Quizz) {
	t.Helper()

	if !reflect.DeepEqual(got, want) {
		t.Errorf("got %v, want %v", got, want)
	}
}

func CreateTempFile(t *testing.T, filename string, quizz Quizz) (*os.File, func()) {
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

func AssertNoError(t *testing.T, err error) {
	t.Helper()

	if err != nil {
		t.Fatalf("didn't expect an error but got one, %v", err)
	}
}
