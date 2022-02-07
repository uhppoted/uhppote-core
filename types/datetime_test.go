package types

import (
	"encoding/json"
	"fmt"
	"testing"
	"time"
)

func TestDateTimeNow(t *testing.T) {
	now := DateTimeNow()
	time.Sleep(100 * time.Millisecond)
	then := DateTimeNow()

	if time.Time(now).Nanosecond() != 0 {
		t.Errorf("DateTimeNow returned non-zero nanoseconds (%v)", now)
	}

	if time.Time(then).Nanosecond() != 0 {
		t.Errorf("DateTimeNow returned non-zero nanoseconds (%v)", then)
	}
}

func TestDateTimeBeforeIgnoresSubseconds(t *testing.T) {
	datetime := DateTime(time.Date(2021, time.February, 28, 12, 34, 56, 345, time.Local))
	reference := time.Date(2021, time.February, 28, 12, 34, 56, 678, time.Local)

	if datetime.Before(reference) {
		t.Errorf("Expected DateTime.Before to ignore subsecond differences")
	}
}

func TestDateTimeAdd(t *testing.T) {
	datetime := DateTime(time.Date(2021, time.February, 28, 12, 34, 56, 789, time.Local))
	expected := DateTime(time.Date(2021, time.February, 28, 15, 34, 56, 789, time.Local).Truncate(1 * time.Second))

	datetime = datetime.Add(3 * time.Hour)

	if datetime != expected {
		t.Errorf("Incorrect date/time - expected:%v, got:%v", expected, datetime)
	}

}

func TestDateTimeMarshalJSON(t *testing.T) {
	utc := time.Date(2021, time.February, 28, 12, 34, 56, 789, time.UTC)
	local := time.Date(2021, time.February, 28, 12, 34, 56, 789, time.Local)
	zero := DateTime{}

	tests := []struct {
		datetime DateTime
		expected string
	}{
		{DateTime(utc), `"2021-02-28 12:34:56 UTC"`},
		{DateTime(local), local.Format(`"2006-01-02 15:04:05 MST"`)},
		{zero, `""`},
	}

	for _, v := range tests {
		if b, err := json.Marshal(v.datetime); err != nil {
			t.Errorf("Error marshalling %v (%v)", v.datetime, err)
		} else if string(b) != v.expected {
			t.Errorf("DateTime %v incorrectly marshalled - expected:%v, got:%v", v.datetime, v.expected, string(b))
		}
	}
}

func TestDateTimeUnmarshalJSON(t *testing.T) {
	utc := DateTime(time.Date(2021, time.February, 28, 12, 34, 56, 789, time.UTC))
	local := DateTime(time.Date(2021, time.February, 28, 12, 34, 56, 789, time.Local))
	zero := DateTime{}

	tests := []struct {
		json     string
		expected DateTime
	}{
		{`"2021-02-28 12:34:56 UTC"`, utc},
		{`"2021-02-28 12:34:56"`, local},
		{`""`, zero},
	}

	for _, v := range tests {
		var dt DateTime

		if err := json.Unmarshal([]byte(v.json), &dt); err != nil {
			t.Errorf("Error unmarshalling %v (%v)", v.json, err)
		} else {
			p := fmt.Sprintf("%v", &dt)
			q := fmt.Sprintf("%v", &v.expected)

			if p != q {
				t.Errorf("Invalid date/time - expected:%v, got:%v", q, p)
			}
		}
	}
}

func TestDateTimeString(t *testing.T) {
	var zero = DateTime{}
	var datetime = DateTime(time.Date(2021, time.February, 28, 12, 34, 56, 789, time.Local))

	tests := []struct {
		dt       interface{}
		expected string
	}{
		{datetime, "2021-02-28 12:34:56"},
		{&datetime, "2021-02-28 12:34:56"},
		{zero, ""},
		{&zero, ""},
	}

	for _, v := range tests {
		s := fmt.Sprintf("%v", v.dt)

		if s != v.expected {
			t.Errorf("Invalid date/time string - expected:%v, got:%v", v.expected, s)
		}
	}
}
