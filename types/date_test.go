package types

import (
	"testing"
	"time"
)

func TestDateBeforeToday(t *testing.T) {
	today := Date(time.Now())
	date, _ := DateFromString(today.String())

	if date.Before(today) {
		t.Errorf("date '%v' should not be before today (%v)", date, today)
	}

	if today.Before(*date) {
		t.Errorf("today (%v) should not be before date '%v'", today, date)
	}
}

func TestDateAfterToday(t *testing.T) {
	today := Date(time.Now())
	date, _ := DateFromString(today.String())

	if date.After(today) {
		t.Errorf("date '%v' should not be after today (%v)", date, today)
	}

	if today.After(*date) {
		t.Errorf("today (%v) should not be after date '%v'", today, date)
	}
}
