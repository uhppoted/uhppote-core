package types

import (
	"fmt"
	"testing"
	"time"
)

func TestStatusToString(t *testing.T) {
	datetime := func(y int, m time.Month, d int, hh int, mm int, ss int) *DateTime {
		dt := DateTime(time.Date(y, m, d, hh, mm, ss, 0, time.Local))

		return &dt
	}

	tests := []struct {
		status   Status
		expected string
	}{
		{
			status: Status{
				SerialNumber:   423187757,
				DoorState:      map[uint8]bool{1: true, 2: true, 3: true, 4: true},
				DoorButton:     map[uint8]bool{1: false, 2: false, 3: false, 4: false},
				SystemError:    0,
				SystemDateTime: datetime(2023, time.January, 25, 11, 20, 28),
				SequenceId:     0,
				SpecialInfo:    0,
				RelayState:     0,
				InputState:     0,
				Event: &StatusEvent{
					Index:      207405,
					Type:       1,
					Granted:    false,
					Door:       3,
					Direction:  1,
					CardNumber: 10058399,
					Timestamp:  datetime(2023, time.January, 25, 11, 20, 28),
					Reason:     5,
				},
			},
			expected: "423187757  true  true  true  true  false false false false 0    2023-01-25 11:20:28 0          0 00 00 | 207405  1   false 3 1     10058399   2023-01-25 11:20:28 5",
		},

		{
			status: Status{
				SerialNumber:   405419896,
				DoorState:      map[uint8]bool{1: false, 2: false, 3: false, 4: false},
				DoorButton:     map[uint8]bool{1: false, 2: false, 3: false, 4: false},
				SystemError:    0,
				SystemDateTime: datetime(2023, time.January, 25, 12, 30, 24),
				SequenceId:     0,
				SpecialInfo:    0,
				RelayState:     0,
				InputState:     0,
				Event: &StatusEvent{
					Index:      257,
					Type:       1,
					Granted:    false,
					Door:       3,
					Direction:  1,
					CardNumber: 8165538,
					Timestamp:  datetime(2023, time.January, 25, 12, 30, 24),
					Reason:     5,
				},
			},
			expected: "405419896  false false false false false false false false 0    2023-01-25 12:30:24 0          0 00 00 | 257     1   false 3 1     8165538    2023-01-25 12:30:24 5",
		},
	}

	for _, test := range tests {
		s := fmt.Sprintf("%v", test.status)

		if s != test.expected {
			t.Errorf("Incorrectly formatted status\n   expected:%v\n   got:     %v", test.expected, s)
		}
	}

}
