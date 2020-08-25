package cyoa

import (
	"encoding/json"
	"fmt"
	"os"
)

type Story map[string]Chapter

type Chapter struct {
	Title      string   `json:"title"`
	Paragraphs []string `json:"story"`
	Options    []Option `json:"options"`
}

type Option struct {
	Text string `json:"text"`
	Arc  string `json:"arc"`
}

func NewStory(file *os.File) (Story, error) {
	var story Story

	err := initialiseStoryFile(file)

	if err != nil {
		return nil, fmt.Errorf("Problem initialising story file, %v", err)
	}

	err = json.NewDecoder(file).Decode(&story)

	if err != nil {
		return nil, fmt.Errorf("Failed to read story from the JSON: %v", err)
	}

	return story, nil
}

func initialiseStoryFile(file *os.File) error {
	file.Seek(0, 0)

	info, err := file.Stat()

	if err != nil {
		return fmt.Errorf("Problem getting file info from file %s, %v", file.Name(), err)
	}

	if info.Size() == 0 {
		file.Write([]byte("{}"))
		file.Seek(0, 0)
	}

	return nil
}
