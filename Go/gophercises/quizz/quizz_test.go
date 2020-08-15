package quizz_test

import (
	"encoding/csv"
	"os"
	"reflect"
	"testing"

	"github.com/herulume/quizz"
)

func TestQuizz(t *testing.T) {

	t.Run("it loads the default quizz", func(t *testing.T) {
		_, err := quizz.LoadDefaultQuizz()

		assertNoError(t, err)
	})

	t.Run("it loads a quizz from a file", func(t *testing.T) {
		wantedQuizz := quizz.Quizz{
			{"1+1", "2"},
			{"2*2", "4"},
			{"Obj(Cat)", "Small categories"},
		}
		quizzFile := "test.csv"

		_, closeF := createTempFile(t, quizzFile, wantedQuizz)
		defer closeF()

		got, err := quizz.LoadQuizz(quizzFile)

		assertNoError(t, err)
		assertQuizz(t, got, wantedQuizz)
	})
}

func assertQuizz(t *testing.T, got, want quizz.Quizz) {
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
	for _, round := range quizz {
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
