package types

import (
	"testing"
	"time"
)

func TestDateBefore(t *testing.T) {
	tests := []struct {
		p        Date
		q        Date
		expected bool
	}{
		{ToDate(2020, time.May, 5), ToDate(2021, time.May, 5), true},
		{ToDate(2021, time.May, 5), ToDate(2021, time.May, 5), false},
		{ToDate(2022, time.May, 5), ToDate(2021, time.May, 5), false},

		{ToDate(2021, time.April, 5), ToDate(2021, time.May, 5), true},
		{ToDate(2021, time.May, 5), ToDate(2021, time.May, 5), false},
		{ToDate(2021, time.June, 5), ToDate(2021, time.May, 5), false},

		{ToDate(2021, time.May, 4), ToDate(2021, time.May, 5), true},
		{ToDate(2021, time.May, 5), ToDate(2021, time.May, 5), false},
		{ToDate(2021, time.May, 6), ToDate(2021, time.May, 5), false},
	}

	for _, v := range tests {
		if before := v.p.Before(v.q); before != v.expected {
			t.Errorf("Expected %v %v 'before' %v, got:%v", v.expected, v.p, v.q, before)
		}
	}
}

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

func TestDateAfter(t *testing.T) {
	tests := []struct {
		p        Date
		q        Date
		expected bool
	}{
		{ToDate(2020, time.May, 5), ToDate(2021, time.May, 5), false},
		{ToDate(2021, time.May, 5), ToDate(2021, time.May, 5), false},
		{ToDate(2022, time.May, 5), ToDate(2021, time.May, 5), true},

		{ToDate(2021, time.April, 5), ToDate(2021, time.May, 5), false},
		{ToDate(2021, time.May, 5), ToDate(2021, time.May, 5), false},
		{ToDate(2021, time.June, 5), ToDate(2021, time.May, 5), true},

		{ToDate(2021, time.May, 4), ToDate(2021, time.May, 5), false},
		{ToDate(2021, time.May, 5), ToDate(2021, time.May, 5), false},
		{ToDate(2021, time.May, 6), ToDate(2021, time.May, 5), true},
	}

	for _, v := range tests {
		if after := v.p.After(v.q); after != v.expected {
			t.Errorf("Expected %v %v 'after' %v, got:%v", v.expected, v.p, v.q, after)
		}
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
