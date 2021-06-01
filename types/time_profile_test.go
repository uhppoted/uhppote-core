package types

import (
	"encoding/json"
	"fmt"
	"reflect"
	"testing"
	"time"
)

func TestTimeProfileToString(t *testing.T) {
	expected := "4 2021-04-01:2021-12-29 Mon,Tue,Thurs,Sat,Sun 08:30-09:45,11:35-13:15,14:01-17:59 19"

	from := date("2021-04-01")
	to := date("2021-12-29")
	profile := TimeProfile{
		ID:              4,
		LinkedProfileID: 19,
		From:            &from,
		To:              &to,
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
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

func TestTimeProfileJSONMarshal(t *testing.T) {
	expected := `{
  "id": 4,
  "linked-profile": 19,
  "start-date": "2021-04-01",
  "end-date": "2021-12-29",
  "weekdays": "Monday,Tuesday,Thursday,Saturday,Sunday",
  "segments": [
    {
      "start": "08:30",
      "end": "09:45"
    },
    {
      "start": "11:35"
    },
    {
      "end": "17:59"
    }
  ]
}`

	from := date("2021-04-01")
	to := date("2021-12-29")

	profile := TimeProfile{
		ID:              4,
		LinkedProfileID: 19,
		From:            &from,
		To:              &to,
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: Segments{
			1: Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: Segment{Start: hhmm("11:35")},
			3: Segment{End: hhmm("17:59")},
		},
	}

	bytes, err := json.MarshalIndent(profile, "", "  ")
	if err != nil {
		t.Fatalf("Error marshalling TimeProfile (%v)", err)
	}

	if string(bytes) != expected {
		t.Errorf("TimeProfile incorrectly marshalled\n   expected:%v\n   got:     %v", expected, string(bytes))
	}
}

func TestTimeProfileJSONUnmarshal(t *testing.T) {
	from := date("2021-04-01")
	to := date("2021-12-29")

	expected := TimeProfile{
		ID:              4,
		LinkedProfileID: 19,
		From:            &from,
		To:              &to,
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
		Segments: Segments{
			1: Segment{Start: hhmm("08:30"), End: hhmm("09:45")},
			2: Segment{Start: hhmm("11:35")},
			3: Segment{End: hhmm("17:59")},
		},
	}

	bytes := []byte(`{
  "id": 4,
  "linked-profile": 19,
  "start-date": "2021-04-01",
  "end-date": "2021-12-29",
  "weekdays": "Monday,Tuesday,Thursday,Saturday,Sunday",
  "segments": [
    {
      "start": "08:30",
      "end": "09:45"
    },
    {
      "start": "11:35"
    },
    {
      "end": "17:59"
    }
  ]
}`)

	var profile TimeProfile

	if err := json.Unmarshal(bytes, &profile); err != nil {
		t.Fatalf("Error unmarshalling TimeProfile (%v)", err)
	}

	if !reflect.DeepEqual(profile, expected) {
		t.Errorf("TimeProfile incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, profile)
	}
}

func hhmm(s string) *HHmm {
	t, _ := HHmmFromString(s)

	return t
}
