package types

import "fmt"

type EventIndex struct {
	SerialNumber SerialNumber
	Index        uint32
}

type EventIndexResult struct {
	SerialNumber SerialNumber
	Index        uint32
	Changed      bool
}

type Event struct {
	SerialNumber SerialNumber
	Index        uint32
	Type         byte
	Granted      bool
	Door         byte
	Direction    uint8
	CardNumber   uint32
	Timestamp    DateTime
	Reason       uint8
}

func (s *EventIndex) String() string {
	return fmt.Sprintf("%s %d", s.SerialNumber, s.Index)
}

func (s *EventIndexResult) String() string {
	return fmt.Sprintf("%s %-8d %v", s.SerialNumber, s.Index, s.Changed)
}

func (s *Event) String() string {
	return fmt.Sprintf("%s %-6d %s %-12d %1d %-5v %d", s.SerialNumber, s.Index, s.Timestamp.String(), s.CardNumber, s.Door, s.Granted, s.Reason)
}
