package quizz

import (
	"encoding/csv"
	"os"
	"testing"
)


func CreateTempFile(t *testing.T, filename string, quizz []Round) (*os.File, func()) {
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

func AssertNoError(t *testing.T, err error) {
	t.Helper()

	if err != nil {
		t.Fatalf("didn't expect an error but got one, %v", err)
	}
}
