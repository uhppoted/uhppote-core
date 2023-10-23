package types

import (
	"fmt"
	"testing"
	"time"
)

func TestEventIsZeroWithZeroEvent(t *testing.T) {
	datetime := func(y int, m time.Month, d int, hh int, mm int, ss int) DateTime {
		return DateTime(time.Date(y, m, d, hh, mm, ss, 0, time.Local))
	}

	event := Event{
		SerialNumber: 405419896,
		Index:        0,
		Type:         1,
		Granted:      false,
		Door:         3,
		Direction:    1,
		CardNumber:   10058399,
		Timestamp:    datetime(2023, time.January, 25, 11, 20, 28),
		Reason:       5,
	}

	if isZero := event.IsZero(); !isZero {
		t.Errorf("'zero value' event returned %v for IsZero()", isZero)
	}
}

func TestEventIsZeroWithNonZeroEvent(t *testing.T) {
	datetime := func(y int, m time.Month, d int, hh int, mm int, ss int) DateTime {
		return DateTime(time.Date(y, m, d, hh, mm, ss, 0, time.Local))
	}

	event := Event{
		SerialNumber: 405419896,
		Index:        12345,
		Type:         1,
		Granted:      false,
		Door:         3,
		Direction:    1,
		CardNumber:   10058399,
		Timestamp:    datetime(2023, time.January, 25, 11, 20, 28),
		Reason:       5,
	}

	if isZero := event.IsZero(); isZero {
		t.Errorf("'zero value' event returned %v for IsZero()", isZero)
	}
}

func TestEventToString(t *testing.T) {
	datetime := func(y int, m time.Month, d int, hh int, mm int, ss int) DateTime {
		return DateTime(time.Date(y, m, d, hh, mm, ss, 0, time.Local))
	}

	tests := []struct {
		event    Event
		expected string
	}{
		{
			event: Event{
				SerialNumber: 405419896,
				Index:        207405,
				Type:         1,
				Granted:      false,
				Door:         3,
				Direction:    1,
				CardNumber:   10058399,
				Timestamp:    datetime(2023, time.January, 25, 11, 20, 28),
				Reason:       5,
			},
			expected: "405419896  207405 2023-01-25 11:20:28 10058399     3 false 5",
		},

		{
			event: Event{
				SerialNumber: 405419896,
				Index:        257,
				Type:         1,
				Granted:      false,
				Door:         3,
				Direction:    1,
				CardNumber:   8165538,
				Timestamp:    datetime(2023, time.January, 25, 12, 30, 24),
				Reason:       5,
			},
			expected: "405419896  257    2023-01-25 12:30:24 8165538      3 false 5",
		},

		{
			event: Event{
				SerialNumber: 405419896,
				Index:        0,
				Type:         1,
				Granted:      false,
				Door:         3,
				Direction:    1,
				CardNumber:   8165538,
				Timestamp:    datetime(2023, time.January, 25, 12, 30, 24),
				Reason:       5,
			},
			expected: "",
		},
	}

	for _, test := range tests {
		s := fmt.Sprintf("%v", test.event)

		if s != test.expected {
			t.Errorf("Incorrectly formatted event\n   expected:%v\n   got:     %v", test.expected, s)
		}
	}
}
