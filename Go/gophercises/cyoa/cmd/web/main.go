package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"

	cyoa "github.com/herulume/cyoa/pkg/story"
)

func main() {
	filename := flag.String("file", "gopher.json", "the JSON file with the story")
	port := flag.Int("port", 3000, "the port to start the web application on")
	flag.Parse()
	fmt.Printf("Using the story in %s\n", *filename)

	file, err := os.Open(*filename)
	defer file.Close()

	if err != nil {
		log.Fatal(err)
		fmt.Fprint(os.Stderr, "Failed to open the JSON with the story")
		return
	}

	story, err := cyoa.NewStory(file)

	if err != nil {
		log.Fatal(err)
		fmt.Fprint(os.Stderr, "Failed to create a story from the file.")
		return
	}

	h := cyoa.NewStoryServer(story)
	fmt.Printf("Starting the server on port %d\n", *port)

	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%d", *port), h))
}
