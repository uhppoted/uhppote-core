package types

import (
	"encoding/json"
	"testing"
)

func hhmm(s string) HHmm {
	t, _ := HHmmFromString(s)

	return *t
}

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

func TestHHmmBefore(t *testing.T) {
	tests := []struct {
		p        HHmm
		q        HHmm
		expected bool
	}{
		{NewHHmm(10, 30), NewHHmm(11, 30), true},
		{NewHHmm(11, 30), NewHHmm(11, 30), false},
		{NewHHmm(12, 30), NewHHmm(11, 30), false},

		{NewHHmm(11, 25), NewHHmm(11, 30), true},
		{NewHHmm(11, 30), NewHHmm(11, 30), false},
		{NewHHmm(11, 35), NewHHmm(11, 30), false},
	}

	for _, v := range tests {
		if before := v.p.Before(v.q); before != v.expected {
			t.Errorf("Expected %v %v 'before' %v, got:%v", v.expected, v.p, v.q, before)
		}
	}
}

func TestHHmmAfter(t *testing.T) {
	tests := []struct {
		p        HHmm
		q        HHmm
		expected bool
	}{
		{NewHHmm(10, 30), NewHHmm(11, 30), false},
		{NewHHmm(11, 30), NewHHmm(11, 30), false},
		{NewHHmm(12, 30), NewHHmm(11, 30), true},

		{NewHHmm(11, 25), NewHHmm(11, 30), false},
		{NewHHmm(11, 30), NewHHmm(11, 30), false},
		{NewHHmm(11, 35), NewHHmm(11, 30), true},
	}

	for _, v := range tests {
		if after := v.p.After(v.q); after != v.expected {
			t.Errorf("Expected %v %v 'after' %v, got:%v", v.expected, v.p, v.q, after)
		}
	}
}
