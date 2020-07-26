package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
)

func Greet(writer io.Writer, name string) {
	fmt.Fprintf(writer, "Hello, %s", name)
}

func MyGreeterHandler(w http.ResponseWriter, r *http.Request) {
	Greet(w, "world")
}

func MyGreeterStdO(name string) {
	Greet(os.Stdout, name)
}

func main() {
	MyGreeterStdO("herulume")
	err := http.ListenAndServe(":5000", http.HandlerFunc(MyGreeterHandler))

	if err != nil {
		fmt.Println(err)
	}
}
