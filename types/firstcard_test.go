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
