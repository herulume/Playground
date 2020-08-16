package quizz_test

import (
	"bytes"
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
		wantedQuizz.Start(&QuestionsSpy)

		quizz.AssertQuestions(t, QuestionsSpy, &wantedQuizz)
	})
}
