package types

import (
	"encoding/json"
	"testing"
)

func TestHHmmMarshalJSON(t *testing.T) {
	expected := `"08:31"`
	hhmm := NewHHmm(8, 31)

	bytes, err := json.Marshal(hhmm)
	if err != nil {
		t.Fatalf("Error marshalling HHmm (%v)", err)
	}

	if string(bytes) != expected {
		t.Errorf("Incorrectly marshalled HHmm - expected:%v, got:%v", expected, string(bytes))
	}
}

func TestHHmmUnmarshalJSON(t *testing.T) {
	expected := NewHHmm(8, 31)
	hhmm := HHmm{}

	err := json.Unmarshal([]byte(`"08:31"`), &hhmm)
	if err != nil {
		t.Fatalf("Error marshalling HHmm (%v)", err)
	}

	if hhmm != expected {
		t.Errorf("Incorrectly unmarshalled HHmm - expected:%v, got:%v", expected, hhmm)
	}
}
