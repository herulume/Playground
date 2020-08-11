package main

import (
	"os"

	poker "github.com/herulume/learn-go-tdd/time/v2"
)

const dbFileName = "game.db.json"

func main() {
	game := poker.NewGame(poker.BlindAlerterFunc(poker.StdOutAlerter), store)
	cli := poker.NewCLI(os.Stdin, os.Stdout, game)
	cli.PlayPoker()
}
