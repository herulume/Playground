package main

import (
	"bytes"
	"flag"
	"fmt"
	"net/http"
	"os"

	"github.com/herulume/urlshort"
)

func main() {
	mux := defaultMux()

	var yaml = flag.String("yaml", "urls.yaml", "urls in yaml format")
	flag.Parse()

	yamlFile, err := os.Open(*yaml)
	defer yamlFile.Close()

	if err != nil {
		panic(err)
	}

	// Build the MapHandler using the mux as the fallback
	pathsToUrls := map[string]string{
		"/urlshort-godoc": "https://godoc.org/github.com/gophercises/urlshort",
		"/yaml-godoc":     "https://godoc.org/gopkg.in/yaml.v2",
	}
	mapHandler := urlshort.MapHandler(pathsToUrls, mux)

	yamlHandler, err := urlshort.YAMLHandler(getFileBytes(yamlFile), mapHandler)

	if err != nil {
		panic(err)
	}

	fmt.Println("Starting the server on :8080")
	http.ListenAndServe(":8080", yamlHandler)
}

func defaultMux() *http.ServeMux {
	mux := http.NewServeMux()
	mux.HandleFunc("/", hello)
	return mux
}

func getFileBytes(f *os.File) []byte {
	buf := bytes.Buffer{}

	_, err := buf.ReadFrom(f)

	if err != nil {
		panic(err)
	}

	return buf.Bytes()
}

func hello(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintln(w, "Hello, world!")
}
