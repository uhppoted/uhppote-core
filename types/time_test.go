package types

import (
	"encoding/json"
	"testing"
	"time"
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

func TestHHmmBeforeNow(t *testing.T) {
	now := time.Now()
	hhmm, _ := HHmmFromString(now.Format("15:04"))

	if hhmm.Before(now) {
		t.Errorf("HHmm '%v' should not be before now (%v)", hhmm, now)
	}
}

func TestHHmmAfterNow(t *testing.T) {
	now := time.Now()
	hhmm, _ := HHmmFromString(now.Format("15:04"))

	if hhmm.After(now) {
		t.Errorf("HHmm '%v' should not be after now (%v)", hhmm, now)
	}
}
