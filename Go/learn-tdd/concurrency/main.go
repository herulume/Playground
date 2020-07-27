package main

import (
	"fmt"
	"github.com/herulume/learn-go-tdd/concurrency/v2"
)

func main() {
	urls := []string{"http://sapo.pt", "https://facebook.com", "di.uminho.pt"}
	m := concurrency.CheckWebsites(concurrency.CheckWebsite, urls)

	for k, v := range m {
		fmt.Printf("%s is %v\n", k, v)
	}

}
