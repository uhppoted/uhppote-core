package types

import (
	"encoding/json"
	"fmt"
	"strings"
)

type FirstCard struct {
	Door      uint8       `json:"door,omitempty"`
	Start     HHmm        `json:"start,omitempty"`
	StartMode DoorControl `json:"start-mode"`
	End       HHmm        `json:"end,omitempty"`
	EndMode   DoorControl `json:"end-mode"`
	Weekdays  Weekdays    `json:"weekdays,omitempty"`
}

type DoorControl int

const (
	Controlled DoorControl = iota
	Open
	Closed
	FirstCardOnly
)

func (d DoorControl) String() string {
	return [...]string{
		"DOOR CONTROLLED",
		"DOOR OPEN",
		"DOOR CLOSED",
		"FIRST CARD ONLY",
	}[d]
}

func (f FirstCard) String() string {
	door := fmt.Sprintf("%v", f.Door)
	start := fmt.Sprintf("%v", f.Start)
	startControl := fmt.Sprintf("%v", f.StartMode)
	end := fmt.Sprintf("%v", f.End)
	endControl := fmt.Sprintf("%v", f.EndMode)
	weekdays := fmt.Sprintf("%v", f.Weekdays)

	list := []string{}
	for _, s := range []string{door, start, startControl, end, endControl, weekdays} {
		if s != "" {
			list = append(list, s)
		}
	}

	return strings.Join(list, " ")
}

func (f *FirstCard) UnmarshalJSON(bytes []byte) error {
	firstcard := struct {
		Door      uint8       `json:"door,omitempty"`
		Start     HHmm        `json:"start,omitempty"`
		StartMode DoorControl `json:"start-mode,omitempty"`
		End       HHmm        `json:"end,omitempty"`
		EndMode   DoorControl `json:"end-mode,omitempty"`
		Weekdays  Weekdays    `json:"weekdays,omitempty"`
	}{
		Weekdays: Weekdays{},
	}

	if err := json.Unmarshal(bytes, &firstcard); err != nil {
		return err
	}

	f.Door = firstcard.Door
	f.Start = firstcard.Start
	f.StartMode = firstcard.StartMode
	f.End = firstcard.End
	f.EndMode = firstcard.EndMode
	f.Weekdays = firstcard.Weekdays

	return nil
}
