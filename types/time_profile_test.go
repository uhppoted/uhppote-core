package types

import (
	"fmt"
	"testing"
)

func TestTimeProfileToString(t *testing.T) {
	expected := "4 2021-04-01:2021-12-29 Mon,Tue,Thurs,Sat,Sun 08:30-09:45,11:35-13:15,14:01-17:59 19"

	from := date("2021-04-01")
	to := date("2021-12-29")
	profile := TimeProfile{
		ProfileID:       4,
		LinkedProfileID: 19,
		From:            &from,
		To:              &to,
		Weekdays: Weekdays{
			Monday:    true,
			Tuesday:   true,
			Wednesday: false,
			Thursday:  true,
			Friday:    false,
			Saturday:  true,
			Sunday:    true,
		},
		Segments: Segments{
			1: Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: Segment{Start: hhmm("11:35"), End: hhmm("13:15")},
			3: Segment{Start: hhmm("14:01"), End: hhmm("17:59")},
		},
	}

	s := fmt.Sprintf("%v", profile)

	if s != expected {
		t.Errorf("TimeProfile incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func hhmm(s string) *HHmm {
	t, _ := HHmmFromString(s)

	return t
}
