package cyoa

import (
	"io/ioutil"
	"os"
	"reflect"
	"testing"
)

func TestNewStory(t *testing.T) {

	t.Run("it creates a story from a json", func(t *testing.T) {
		f, cleanFile := createTempFile(t, wantedJsonText)
		defer cleanFile()

		got, err := NewStory(f)

		assertNoError(t, err)
		assertStory(t, got, wantedJson)
	})

	t.Run("it works with an empty file", func(t *testing.T) {
		f, cleanFile := createTempFile(t, "")
		defer cleanFile()

		_, err := NewStory(f)

		assertNoError(t, err)
	})
}

func assertNoError(t *testing.T, err error) {
	t.Helper()

	if err != nil {
		t.Fatalf("Got an error, didn't expect one: %v", err)
	}
}

func assertStory(t *testing.T, got, want Story) {
	t.Helper()

	if !reflect.DeepEqual(want, got) {
		t.Errorf("Expected %v, got %v", want, got)
	}
}

func createTempFile(t *testing.T, initialData string) (*os.File, func()) {
	t.Helper()

	tmpfile, err := ioutil.TempFile("", "db")

	if err != nil {
		t.Fatalf("could not create temp file %v", err)
	}

	tmpfile.Write([]byte(initialData))
	tmpfile.Seek(0, 0)

	removeFile := func() {
		tmpfile.Close()
		os.Remove(tmpfile.Name())
	}

	return tmpfile, removeFile
}

var wantedJson = Story{
	"Chapter 2": Chapter{
		Title: "The running",
		Paragraphs: []string{
			"Run!!!!",
		},
		Options: []Option{
			{
				Text: "Try again!",
				Arc:  "Intro",
			},
		},
	},
}

const wantedJsonText = `{
  "Chapter 2":{
	"title":"The running", "story":["Run!!!!"], "options":[
	  {"text":"Try again!", "arc":"Intro"}
	]
  }
 }`
