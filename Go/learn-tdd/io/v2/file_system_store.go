package main

import (
	"io"
)

// FileSystemPlayerStore collects data about players in disk.
type FileSystemPlayerStore struct {
	database io.ReadSeeker
	// A mutex is used to synchronize read/write access to the map
	//lock sync.RWMutex
}

func (f *FileSystemPlayerStore) GetLeague() []Player {
	f.database.Seek(0, 0)
	league, _ := Newleague(f.database)
	return league
}

func (f *FileSystemPlayerStore) GetPlayerScore(name string) int {
	var wins int

	for _, player := range f.GetLeague() {
		if player.Name == name {
			wins = player.Wins
			break
		}
	}

	return wins
}
