package main

import (
	"io"
)

// FileSystemPlayerStore collects data about players in disk.
type FileSystemPlayerStore struct {
	database io.Reader
	// A mutex is used to synchronize read/write access to the map
	//lock sync.RWMutex
}

func (f *FileSystemPlayerStore) GetLeague() []Player {
	league, _ := Newleague(f.database)
	return league
}
