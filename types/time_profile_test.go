package types

import (
	"encoding/json"
	"fmt"
	"reflect"
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

func TestTimeProfileJSONMarshal(t *testing.T) {
	expected := `{
  "profile-id": 4,
  "linked-profile-id": 19,
  "start-date": "2021-04-01",
  "end-date": "2021-12-29",
  "weekdays": "Monday,Tuesday,Thursday,Saturday,Sunday",
  "segments": {
    "1": {
      "start": "08:30",
      "end": "09:45"
    },
    "2": {
      "start": "11:35"
    },
    "3": {
      "end": "17:59"
    }
  }
}`

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

func TestWeekdaysMarshalJSON(t *testing.T) {
	expected := `"Monday,Wednesday,Thursday,Saturday,Sunday"`
	weekdays := Weekdays{
		Monday:    true,
		Tuesday:   false,
		Wednesday: true,
		Thursday:  true,
		Friday:    false,
		Saturday:  true,
		Sunday:    true,
	}

	bytes, err := json.Marshal(weekdays)
	if err != nil {
		t.Fatalf("Error marshalling weekdays (%v)", err)
	}

	if string(bytes) != expected {
		t.Errorf("Incorrectly marshalled weekdays - expected:%v, got:%v", expected, string(bytes))
	}
}

func TestWeekdaysUnmarshalJSON(t *testing.T) {
	expected := Weekdays{
		Monday:    true,
		Tuesday:   false,
		Wednesday: true,
		Thursday:  true,
		Friday:    false,
		Saturday:  true,
		Sunday:    true,
	}

	weekdays := Weekdays{}

	if err := json.Unmarshal([]byte(`"Monday,Wednesday,Thursday,Saturday,Sunday"`), &weekdays); err != nil {
		t.Fatalf("Error unmarshalling weekdays (%v)", err)
	}

	if !reflect.DeepEqual(weekdays, expected) {
		t.Errorf("Incorrectly unmarshalled weekdays - expected:%v, got:%v", expected, weekdays)
	}
}

func TestWeekdayMarshalJSON(t *testing.T) {
	tests := []struct {
		weekday  Weekday
		expected string
	}{
		{weekday: Monday, expected: `"Monday"`},
		{weekday: Tuesday, expected: `"Tuesday"`},
		{weekday: Wednesday, expected: `"Wednesday"`},
		{weekday: Thursday, expected: `"Thursday"`},
		{weekday: Friday, expected: `"Friday"`},
		{weekday: Saturday, expected: `"Saturday"`},
		{weekday: Sunday, expected: `"Sunday"`},
	}

	for _, v := range tests {
		bytes, err := json.Marshal(v.weekday)
		if err != nil {
			t.Fatalf("Error marshalling weekday (%v)", err)
		}

		if string(bytes) != v.expected {
			t.Errorf("Incorrectly marshalled weekday - expected:%v, got:%v", v.expected, string(bytes))
		}
	}
}

func TestWeekdayUnmarshalJSON(t *testing.T) {
	tests := []struct {
		bytes    []byte
		expected Weekday
	}{
		{bytes: []byte(`"Monday"`), expected: Monday},
		{bytes: []byte(`"Tuesday"`), expected: Tuesday},
		{bytes: []byte(`"Wednesday"`), expected: Wednesday},
		{bytes: []byte(`"Thursday"`), expected: Thursday},
		{bytes: []byte(`"Friday"`), expected: Friday},
		{bytes: []byte(`"Saturday"`), expected: Saturday},
		{bytes: []byte(`"Sunday"`), expected: Sunday},

		{bytes: []byte(`"monday"`), expected: Monday},
		{bytes: []byte(`"tuesday"`), expected: Tuesday},
		{bytes: []byte(`"wednesday"`), expected: Wednesday},
		{bytes: []byte(`"thursday"`), expected: Thursday},
		{bytes: []byte(`"friday"`), expected: Friday},
		{bytes: []byte(`"saturday"`), expected: Saturday},
		{bytes: []byte(`"sunday"`), expected: Sunday},
	}

	for _, v := range tests {
		var weekday Weekday
		if err := json.Unmarshal(v.bytes, &weekday); err != nil {
			t.Fatalf("Error unmarshalling weekday (%v)", err)
		}

		if weekday != v.expected {
			t.Errorf("Incorrectly unmarshalled weekday - expected:%v, got:%v", v.expected, weekday)
		}
	}
}

func TestWeekdayUnmarshalJSONWithInvalidValue(t *testing.T) {
	var weekday Weekday
	if err := json.Unmarshal([]byte(`"adfasdf"`), &weekday); err == nil {
		t.Errorf("Expected error unmarshalling 'asdfasdf', got:%v", err)
	}
}

func hhmm(s string) *HHmm {
	t, _ := HHmmFromString(s)

	return t
}
