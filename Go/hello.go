package main

import (
	"fmt"
	"time"
)

type Person struct {
	Name string
	Age  int
}

func (p *Person) SayHello() {
	fmt.Println("Hello,", p.Name)
}

type Friend interface {
	SayHello()
}

func Greet(f Friend) {
	f.SayHello()
}

func f() {
	fmt.Println("GO")
}

func strlen(s string, c chan int) {
	c <- len(s)
}

func main() {
	var s = make([]string, 0)
	var m = make(map[string]string)

	go f()

	time.Sleep(1 * time.Second)

	s = append(s, "some string")
	m["some key"] = "some value"

	fmt.Println(s)
	fmt.Println(m)

	var count = int(42)
	ptr := &count

	fmt.Println(*ptr)
	*ptr = 100
	fmt.Println(count)

	var guy = new(Person)
	guy.Name = "Cona"
	guy.SayHello()
	Greet(guy)

	c := make(chan int)
	go strlen("Salutations", c)
	go strlen("World", c)
	x, y := <-c, <-c

	fmt.Println(x, y, x+y)
}
