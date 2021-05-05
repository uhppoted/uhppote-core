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

func TestHHmmBefore(t *testing.T) {
	now := time.Now()
	y := now.Year()
	m := now.Month()
	d := now.Day()
	s := now.Second()
	ns := now.Nanosecond()

	tests := []struct {
		p        HHmm
		q        time.Time
		expected bool
	}{
		{NewHHmm(10, 30), time.Date(y, m, d, 11, 30, s, ns, time.Local), true},
		{NewHHmm(11, 30), time.Date(y, m, d, 11, 30, s, ns, time.Local), false},
		{NewHHmm(12, 30), time.Date(y, m, d, 11, 30, s, ns, time.Local), false},

		{NewHHmm(11, 25), time.Date(y, m, d, 11, 30, s, ns, time.Local), true},
		{NewHHmm(11, 30), time.Date(y, m, d, 11, 30, s, ns, time.Local), false},
		{NewHHmm(11, 35), time.Date(y, m, d, 11, 30, s, ns, time.Local), false},
	}

	for _, v := range tests {
		if before := v.p.Before(v.q); before != v.expected {
			t.Errorf("Expected %v %v 'before' %v, got:%v", v.expected, v.p, v.q, before)
		}
	}
}

func TestHHmmBeforeNow(t *testing.T) {
	now := time.Now()
	hhmm, _ := HHmmFromString(now.Format("15:04"))

	if hhmm.Before(now) {
		t.Errorf("HHmm '%v' should not be before now (%v)", hhmm, now)
	}
}

func TestHHmmAfter(t *testing.T) {
	now := time.Now()
	y := now.Year()
	m := now.Month()
	d := now.Day()
	s := now.Second()
	ns := now.Nanosecond()

	tests := []struct {
		p        HHmm
		q        time.Time
		expected bool
	}{
		{NewHHmm(10, 30), time.Date(y, m, d, 11, 30, s, ns, time.Local), false},
		{NewHHmm(11, 30), time.Date(y, m, d, 11, 30, s, ns, time.Local), false},
		{NewHHmm(12, 30), time.Date(y, m, d, 11, 30, s, ns, time.Local), true},

		{NewHHmm(11, 25), time.Date(y, m, d, 11, 30, s, ns, time.Local), false},
		{NewHHmm(11, 30), time.Date(y, m, d, 11, 30, s, ns, time.Local), false},
		{NewHHmm(11, 35), time.Date(y, m, d, 11, 30, s, ns, time.Local), true},
	}

	for _, v := range tests {
		if after := v.p.After(v.q); after != v.expected {
			t.Errorf("Expected %v %v 'after' %v, got:%v", v.expected, v.p, v.q, after)
		}
	}
}

func TestHHmmAfterNow(t *testing.T) {
	now := time.Now()
	hhmm, _ := HHmmFromString(now.Format("15:04"))

	if hhmm.After(now) {
		t.Errorf("HHmm '%v' should not be after now (%v)", hhmm, now)
	}
}
