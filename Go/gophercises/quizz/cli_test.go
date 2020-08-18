package quizz_test

import (
	"bytes"
	"testing"

	"github.com/herulume/quizz"
)

func TestCLI(t *testing.T) {

	t.Run("it prints an error when a non existing file is passed", func(t *testing.T) {
		out := &bytes.Buffer{}
		in := &bytes.Buffer{}
		nonExistingFile := "dsf92fss dsf"

		_, err := quizz.NewCLI(nonExistingFile, out, in)

		if err == nil {
			t.Error("Expected an error but got none")
		}

	})
}
