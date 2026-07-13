package types

import (
	"encoding/json"
	"fmt"
	"reflect"
	"testing"
	"time"
)

func TestFirstCardToString(t *testing.T) {
	expected := "active:08:30-16:45  control:normally open/firstcard only  weekdays:Mon,Tue,Thurs,Sat,Sun"

	firstcard := FirstCard{
		StartTime: MustParseHHmm("8:30"),
		EndTime:   MustParseHHmm("16:45"),
		Active:    NormallyOpen,
		Inactive:  FirstCardOnly,
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	s := fmt.Sprintf("%v", firstcard)

	if s != expected {
		t.Errorf("FirstCard incorrectly stringified\n   expected:%+v\n   got:     %+v", expected, s)
	}
}

func TestFirstCardIsZero(t *testing.T) {
	tests := []struct {
		firstcard FirstCard
		expected  bool
	}{
		{FirstCard{}, true},
		{FirstCard{StartTime: MustParseHHmm("00:01")}, false},
		{FirstCard{EndTime: MustParseHHmm("00:01")}, false},
		{FirstCard{Active: Controlled}, true},
		{FirstCard{Inactive: Controlled}, true},
		{FirstCard{Weekdays: Weekdays{time.Monday: true}}, false},
		{FirstCard{Weekdays: Weekdays{time.Tuesday: true}}, false},
		{FirstCard{Weekdays: Weekdays{time.Wednesday: true}}, false},
		{FirstCard{Weekdays: Weekdays{time.Thursday: true}}, false},
		{FirstCard{Weekdays: Weekdays{time.Friday: true}}, false},
		{FirstCard{Weekdays: Weekdays{time.Saturday: true}}, false},
		{FirstCard{Weekdays: Weekdays{time.Sunday: true}}, false},
	}

	for _, test := range tests {
		if zero := test.firstcard.IsZero(); zero != test.expected {
			t.Errorf("incorrect IsZero for %#v - expected:%v   got:%v", test.firstcard, test.expected, zero)
		}
	}
}

func TestFirstCardJSONMarshal(t *testing.T) {
	expected := `{
  "start-time": "08:30",
  "end-time": "16:45",
  "active-mode": "normally open",
  "inactive-mode": "firstcard only",
  "weekdays": "Monday,Tuesday,Thursday,Saturday,Sunday"
}`

	firstcard := FirstCard{
		StartTime: MustParseHHmm("8:30"),
		EndTime:   MustParseHHmm("16:45"),
		Active:    NormallyOpen,
		Inactive:  FirstCardOnly,
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	bytes, err := json.MarshalIndent(firstcard, "", "  ")
	if err != nil {
		t.Fatalf("Error marshalling FirstCard (%v)", err)
	}

	if string(bytes) != expected {
		t.Errorf("FirstCard incorrectly marshalled\n   expected:%v\n   got:     %v", expected, string(bytes))
	}
}

func TestFirstCardJSONUnmarshal(t *testing.T) {
	expected := FirstCard{
		StartTime: MustParseHHmm("8:30"),
		EndTime:   MustParseHHmm("16:45"),
		Active:    NormallyOpen,
		Inactive:  FirstCardOnly,
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	bytes := []byte(`{
  "start-time": "08:30",
  "end-time": "16:45",
  "active-mode": "normally open",
  "inactive-mode": "firstcard only",
  "weekdays": "Monday,Tuesday,Thursday,Saturday,Sunday"
}`)

	var firstcard FirstCard

	if err := json.Unmarshal(bytes, &firstcard); err != nil {
		t.Fatalf("Error unmarshalling FirstCard (%v)", err)
	}

	if !reflect.DeepEqual(firstcard, expected) {
		t.Errorf("Task incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, firstcard)
	}
}

func TestFirstCardJSONUnmarshalVariant(t *testing.T) {
	expected := FirstCard{
		StartTime: MustParseHHmm("8:30"),
		EndTime:   MustParseHHmm("16:45"),
		Active:    NormallyOpen,
		Inactive:  FirstCardOnly,
		Weekdays: Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	bytes := []byte(`{
  "start-time": "08:30",
  "end-time": "16:45",
  "active-mode": "normally open",
  "inactive-mode": "firstcard",
  "weekdays": "Monday,Tuesday,Thursday,Saturday,Sunday"
}`)

	var firstcard FirstCard

	if err := json.Unmarshal(bytes, &firstcard); err != nil {
		t.Fatalf("Error unmarshalling FirstCard (%v)", err)
	}

	if !reflect.DeepEqual(firstcard, expected) {
		t.Errorf("Task incorrectly unmarshalled\n   expected:%+v\n   got:     %+v", expected, firstcard)
	}
}
