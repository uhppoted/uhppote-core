package types

import "fmt"

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

type EventIndex struct {
	SerialNumber SerialNumber
	Index        uint32
}

type EventIndexResult struct {
	SerialNumber SerialNumber
	Index        uint32
	Changed      bool
}

func (e Event) IsZero() bool {
	return e.Index == 0
}

func (e Event) String() string {
	if e.IsZero() {
		return ""
	} else {
		return fmt.Sprintf("%v %-6v %v %-12v %1v %-5v %v", e.SerialNumber, e.Index, e.Timestamp, e.CardNumber, e.Door, e.Granted, e.Reason)
	}
}

func (e *EventIndex) String() string {
	return fmt.Sprintf("%v %v", e.SerialNumber, e.Index)
}

func (e *EventIndexResult) String() string {
	return fmt.Sprintf("%v %-8v %v", e.SerialNumber, e.Index, e.Changed)
}
