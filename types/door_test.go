package types

import (
	"encoding/json"
	"fmt"
	"testing"
)

func TestDoorControlStateString(t *testing.T) {
	tests := map[ControlState]string{
		NormallyOpen:   "normally open",
		NormallyClosed: "normally closed",
		Controlled:     "controlled",
	}

	for k, v := range tests {
		if s := fmt.Sprintf("%v", k); s != v {
			t.Errorf("Expected %v, got %v", v, s)
		}
	}
}

func TestDoorControlStateMarsall(t *testing.T) {
	tests := map[ControlState]string{
		NormallyOpen:   `"normally open"`,
		NormallyClosed: `"normally closed"`,
		Controlled:     `"controlled"`,
	}

	for k, v := range tests {
		if b, err := json.Marshal(k); err != nil {
			t.Fatalf("Unexpected error (%[2]v) marshalling %[1]v", k, err)
		} else if string(b) != v {
			t.Errorf("Expected %v, got %v", v, string(b))
		}
	}
}

func TestDoorControlStateUnmarsall(t *testing.T) {
	tests := map[ControlState]string{
		NormallyOpen:   `"normally open"`,
		NormallyClosed: `"normally closed"`,
		Controlled:     `"controlled"`,
	}

	for k, s := range tests {
		var v ControlState

		if err := json.Unmarshal([]byte(s), &v); err != nil {
			t.Fatalf("Unexpected error (%[2]v) unmarshalling %[1]v", k, err)
		} else if v != k {
			t.Errorf("Expected %v, got %v", k, v)
		}
	}
}
