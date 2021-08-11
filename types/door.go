package types

import (
	"encoding/json"
	"fmt"
)

type ControlState int

const (
	NormallyOpen ControlState = iota + 1
	NormallyClosed
	Controlled
)

func (v ControlState) String() string {
	return [...]string{"", "normally open", "normally closed", "controlled"}[v]
}

func (v ControlState) MarshalJSON() ([]byte, error) {
	s := [...]string{"", "normally open", "normally closed", "controlled"}[v]

	return json.Marshal(s)
}

func (v *ControlState) UnmarshalJSON(b []byte) error {
	var s string

	if err := json.Unmarshal(b, &s); err != nil {
		return err
	}

	switch s {
	case "normally open":
		*v = NormallyOpen
	case "normally closed":
		*v = NormallyClosed
	case "controlled":
		*v = Controlled

	default:
		return fmt.Errorf("invalid door control state value '%v'", s)
	}

	return nil
}

type DoorControlState struct {
	SerialNumber SerialNumber
	Door         uint8
	ControlState ControlState
	Delay        uint8
}

func (d *DoorControlState) String() string {
	return fmt.Sprintf("%s %v %v %v", d.SerialNumber, d.Door, d.ControlState, d.Delay)
}
