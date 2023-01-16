package types

import (
	"fmt"
	"strings"
)

type Status struct {
	SerialNumber   SerialNumber
	DoorState      map[uint8]bool
	DoorButton     map[uint8]bool
	SystemError    uint8
	SystemDateTime DateTime
	SequenceId     uint32
	SpecialInfo    uint8
	RelayState     uint8
	InputState     uint8
	Event          *StatusEvent
}

type StatusEvent struct {
	Index      uint32
	Type       byte
	Granted    bool
	Door       byte
	Direction  uint8
	CardNumber uint32
	Timestamp  *DateTime
	Reason     uint8
}

func (s *Status) String() string {
	var b strings.Builder

	b.WriteString(fmt.Sprintf("%v", s.SerialNumber))
	b.WriteString(fmt.Sprintf(" %-5v %-5v %-5v %-5v", s.DoorState[1], s.DoorState[2], s.DoorState[3], s.DoorState[4]))
	b.WriteString(fmt.Sprintf(" %-5v %-5v %-5v %-5v", s.DoorButton[1], s.DoorButton[2], s.DoorButton[3], s.DoorButton[4]))
	b.WriteString(fmt.Sprintf(" %-4d", s.SystemError))
	b.WriteString(fmt.Sprintf(" %s", s.SystemDateTime.String()))
	b.WriteString(fmt.Sprintf(" %-10d", s.SequenceId))
	b.WriteString(fmt.Sprintf(" %d", s.SpecialInfo))
	b.WriteString(fmt.Sprintf(" %02X", s.RelayState))
	b.WriteString(fmt.Sprintf(" %02X", s.InputState))

	if s.Event != nil {
		b.WriteString(fmt.Sprintf(" | %-5d", s.Event.Index))
		b.WriteString(fmt.Sprintf(" %-3d", s.Event.Type))
		b.WriteString(fmt.Sprintf(" %-5v", s.Event.Granted))
		b.WriteString(fmt.Sprintf(" %d", s.Event.Door))
		b.WriteString(fmt.Sprintf(" %-5v", s.Event.Direction))
		b.WriteString(fmt.Sprintf(" %-10d", s.Event.CardNumber))
		b.WriteString(fmt.Sprintf(" %v", s.Event.Timestamp))
		b.WriteString(fmt.Sprintf(" %d", s.Event.Reason))
	}

	return b.String()
}
