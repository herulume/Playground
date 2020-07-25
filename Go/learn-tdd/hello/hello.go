package main

import "fmt"

const spanish = "Spanish"
const french = "French"
const quenya = "Quenya"
const englishHelloPrefix = "Hello, "
const spanishHelloPrefix = "Hola, "
const frenchHelloPrefix = "Bonjour, "
const quenyaHelloPrefix = "Aiya, "

func Hello(name, language string) string {
    if name == "" {
        name = "World"
    }

    return greetingPrefix(language) + name
}

func greetingPrefix(language string) (prefix string) {
    switch language {
    case french:
        prefix = frenchHelloPrefix
    case spanish:
        prefix = spanishHelloPrefix
    case quenya:
        prefix = quenyaHelloPrefix
    default:
        prefix = englishHelloPrefix
    }
    return
}

func main() {
    fmt.Println(Hello("herulume", quenya))
}
