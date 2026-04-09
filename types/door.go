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
	FirstCardOnly
)

const (
	ModeUnknown ControlState = iota
	ModeNormallyOpen
	ModeNormallyClosed
	ModeControlled
	ModeFirstCardOnly
)

func (v ControlState) String() string {
	return [...]string{"", "normally open", "normally closed", "controlled", "firstcard"}[v]
}

func (v ControlState) MarshalJSON() ([]byte, error) {
	s := [...]string{"", "normally open", "normally closed", "controlled", "firstcard"}[v]

	return json.Marshal(s)
}

func (v *ControlState) UnmarshalJSON(b []byte) error {
	var s string

	if err := json.Unmarshal(b, &s); err != nil {
		return err
	}

	switch s {
	case "normally open":
		*v = ModeNormallyOpen
	case "normally closed":
		*v = ModeNormallyClosed
	case "controlled":
		*v = ModeControlled
	case "firstcard":
		*v = ModeFirstCardOnly

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
