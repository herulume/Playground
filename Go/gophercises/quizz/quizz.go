package quizz

import (
	"bufio"
	"encoding/csv"
	"fmt"
	"io"
	"os"
	"strings"
)

type Round struct {
	Question string
	Answer   string

}

type Quizz struct {
	Quizz  []Round
	Points uint
}

func NewQuizz(location string) (*Quizz, error) {
	csvFile, err := os.Open(location)
	defer csvFile.Close()

	if err != nil {
		return nil, fmt.Errorf("problem opening quizz's file, %v", err)
	}

	rounds, err := csv.NewReader(csvFile).ReadAll()

	if err != nil {
		return nil, fmt.Errorf("problem reading quizz's questions, %v", err)
	}

	quizz := make([]Round, len(rounds))
	for i, round := range rounds {
		quizz[i] = Round{
			Question: round[0],
			Answer:   strings.TrimSpace(round[1]),
		}
	}

	return &Quizz{quizz, 0}, nil
}

func (q *Quizz) Start(alertsDestination io.Writer, input io.Reader) {
	reader := bufio.NewReader(input)

	for i, r := range q.Quizz {
		fmt.Fprint(alertsDestination, r.FormatQuestion(i))

		got, _ := reader.ReadString('\n')
		q.evalAnswer(got, r.Answer)
	}
}

func (r *Round) FormatQuestion(index int) string {
	return fmt.Sprintf("Problem #%d: %s = ", index+1, r.Question)
}

func (q *Quizz) evalAnswer(got, want string) {
	awnswer := strings.TrimSuffix(got, "\n")
	if awnswer == want {
		q.Points++
	}
}

func (q *Quizz) Finish(alertDestination io.Writer) {
	total := len(q.Quizz)

	fmt.Fprint(alertDestination, "Quizz finished!\n")
	fmt.Fprintf(alertDestination, "Got %d out of %d points\n", q.Points, total)
}
