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
		FirstCardOnly:  "firstcard only",
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
		FirstCardOnly:  `"firstcard only"`,
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
	tests := map[string]ControlState{
		`"normally open"`:   NormallyOpen,
		`"normally closed"`: NormallyClosed,
		`"normally-open"`:   NormallyOpen,
		`"normally-closed"`: NormallyClosed,
		`"controlled"`:      Controlled,
		`"firstcard"`:       FirstCardOnly,
		`"firstcard-only"`:  FirstCardOnly,
		`"firstcard only"`:  FirstCardOnly,
	}

	for k, v := range tests {
		var s ControlState

		if err := json.Unmarshal([]byte(k), &s); err != nil {
			t.Fatalf("Unexpected error (%[2]v) unmarshalling %[1]v", k, err)
		} else if s != v {
			t.Errorf("Expected %v, got %v", v, s)
		}
	}
}
