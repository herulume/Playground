package quizz

import (
	"encoding/csv"
	"fmt"
	"os"
	"strings"
)

type Round struct {
	Question string
	Answer   string
}

type Quizz []Round

const defaultQuizzLocation = "problems.csv"

func LoadDefaultQuizz() (Quizz, error) {
	return LoadQuizz(defaultQuizzLocation)
}

func LoadQuizz(location string) (Quizz, error) {
	csvFile, err := os.Open(location)
	defer csvFile.Close()

	if err != nil {
		return nil, fmt.Errorf("problem opening quizz's file, %v", err)
	}

	rounds, err := csv.NewReader(csvFile).ReadAll()

	if err != nil {
		return nil, fmt.Errorf("problem reading quizz's questions, %v", err)
	}

	var quizz Quizz
	for _, round := range rounds {
		q := Round{
			Question: round[0],
			Answer:   round[1],
		}
		quizz = append(quizz, q)
	}

	return quizz, nil
}

func (r Round) String() string {
	return fmt.Sprintf("Question: %s <*> Answer: %s", r.Question, r.Answer)
}

func (q Quizz) String() string {
	var sb strings.Builder

	for _, r := range q {
		sb.WriteString(r.String())
                sb.WriteString("\n")
	}

	return sb.String()
}
