package types

import (
	"fmt"
	"strings"
)

type Status struct {
	SerialNumber   SerialNumber
	EventIndex     uint32
	EventType      byte
	Granted        bool
	Door           byte
	Direction      uint8
	CardNumber     uint32
	Timestamp      DateTime
	Reason         uint8
	DoorState      map[uint8]bool
	DoorButton     map[uint8]bool
	SystemError    uint8
	SystemDateTime DateTime
	SequenceId     uint32
	SpecialInfo    uint8
	RelayState     uint8
	InputState     uint8
}

func (s *Status) String() string {
	var b strings.Builder

	b.WriteString(fmt.Sprintf("%s", s.SerialNumber))
	b.WriteString(fmt.Sprintf(" %-5d", s.EventIndex))
	b.WriteString(fmt.Sprintf(" %-3d", s.EventType))
	b.WriteString(fmt.Sprintf(" %-5v", s.Granted))
	b.WriteString(fmt.Sprintf(" %d", s.Door))
	b.WriteString(fmt.Sprintf(" %-5v", s.Direction))
	b.WriteString(fmt.Sprintf(" %-10d", s.CardNumber))
	b.WriteString(fmt.Sprintf(" %s", s.Timestamp.String()))
	b.WriteString(fmt.Sprintf(" %-3d", s.Reason))
	b.WriteString(fmt.Sprintf(" %-5v %-5v %-5v %-5v", s.DoorState[1], s.DoorState[2], s.DoorState[3], s.DoorState[4]))
	b.WriteString(fmt.Sprintf(" %-5v %-5v %-5v %-5v", s.DoorButton[1], s.DoorButton[2], s.DoorButton[3], s.DoorButton[4]))
	b.WriteString(fmt.Sprintf(" %-4d", s.SystemError))
	b.WriteString(fmt.Sprintf(" %s", s.SystemDateTime.String()))
	b.WriteString(fmt.Sprintf(" %-10d", s.SequenceId))
	b.WriteString(fmt.Sprintf(" %d", s.SpecialInfo))
	b.WriteString(fmt.Sprintf(" %02X", s.RelayState))
	b.WriteString(fmt.Sprintf(" %02X", s.InputState))

	return b.String()
}
