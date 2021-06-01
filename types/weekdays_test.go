package types

import (
	"encoding/json"
	"reflect"
	"testing"
	"time"
)

func TestWeekdaysMarshalJSON(t *testing.T) {
	expected := `"Monday,Wednesday,Thursday,Saturday,Sunday"`
	weekdays := Weekdays{
		time.Monday:    true,
		time.Tuesday:   false,
		time.Wednesday: true,
		time.Thursday:  true,
		time.Friday:    false,
		time.Saturday:  true,
		time.Sunday:    true,
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
		time.Monday:    true,
		time.Tuesday:   false,
		time.Wednesday: true,
		time.Thursday:  true,
		time.Friday:    false,
		time.Saturday:  true,
		time.Sunday:    true,
	}

	weekdays := Weekdays{}

	if err := json.Unmarshal([]byte(`"Monday,Wednesday,Thursday,Saturday,Sunday"`), &weekdays); err != nil {
		t.Fatalf("Error unmarshalling weekdays (%v)", err)
	}

	if !reflect.DeepEqual(weekdays, expected) {
		t.Errorf("Incorrectly unmarshalled weekdays - expected:%v, got:%v", expected, weekdays)
	}
}
