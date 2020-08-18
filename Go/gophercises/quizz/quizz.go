package quizz

import (
	"bufio"
	"encoding/csv"
	"fmt"
	"io"
	"os"
	"strings"
	"time"
)

type Round struct {
	Question string
	Answer   string
}

type Quizz struct {
	Quizz         []Round
	Points        uint
	TimerDuration time.Duration
}

func NewQuizz(location string, timerDuration time.Duration) (*Quizz, error) {
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

	return &Quizz{
		Quizz:         quizz,
		Points:        0,
		TimerDuration: timerDuration,
	}, nil
}

func (q *Quizz) Start(alertsDestination io.Writer, input io.Reader) {
	reader := bufio.NewReader(input)
	timer := time.NewTimer(q.TimerDuration)

	for i, r := range q.Quizz {
		fmt.Fprint(alertsDestination, r.FormatQuestion(i))
		answerChan := make(chan string)

		go func() {
			got, _ := reader.ReadString('\n')
			answerChan <- got
		}()

		select {
		case <-timer.C:
			fmt.Fprint(alertsDestination, "\n\nTime's out!\n")
			return
		case answer := <-answerChan:
			q.evalAnswer(answer, r.Answer)
		}
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
