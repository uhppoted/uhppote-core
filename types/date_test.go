package types

import (
	"encoding/json"
	"fmt"
	"reflect"
	"testing"
	"time"
)

func TestDateString(t *testing.T) {
	var zero = Date{}
	var date = Date(time.Date(2021, time.February, 28, 12, 34, 56, 789, time.Local))

	tests := []struct {
		dt       interface{}
		expected string
	}{
		{date, "2021-02-28"},
		{&date, "2021-02-28"},
		{zero, ""},
		{&zero, ""},
	}

	for _, v := range tests {
		s := fmt.Sprintf("%v", v.dt)

		if s != v.expected {
			t.Errorf("Invalid date string - expected:%v, got:%v", v.expected, s)
		}
	}
}

func TestDateFromString(t *testing.T) {
	expected := Date(time.Date(2021, time.February, 28, 0, 0, 0, 0, time.Local))

	date, err := ParseDate("2021-02-28")
	if err != nil {
		t.Fatalf("Unexpected error parsing Date (%v)", err)
	}

	if !reflect.DeepEqual(date, expected) {
		t.Errorf("Date incorrectly unmarshaled - expected:%v, got:%v", time.Time(expected), time.Time(date))
	}
}

func TestDateIsZero(t *testing.T) {
	var zero = Date{}
	var date = Date(time.Date(2021, time.February, 28, 12, 34, 56, 789, time.Local))

	tests := []struct {
		date     Date
		expected bool
	}{
		{date, false},
		{zero, true},
	}

	for _, v := range tests {
		if b := v.date.IsZero(); b != v.expected {
			t.Errorf("Date.IsZero returned incorrect value - expected:%v, got:%v", v.expected, date)
		}
	}
}

func TestDateEquals(t *testing.T) {
	tests := []struct {
		date     Date
		other    Date
		expected bool
	}{
		{
			Date(time.Date(2023, time.February, 28, 12, 34, 56, 789, time.Local)),
			Date(time.Date(2023, time.February, 28, 12, 34, 56, 789, time.Local)),
			true},
		{
			Date(time.Date(2023, time.February, 28, 12, 34, 56, 789, time.Local)),
			Date(time.Date(2023, time.February, 28, 12, 34, 0, 0, time.Local)),
			true},
		{
			Date(time.Date(2023, time.February, 28, 12, 34, 56, 789, time.Local)),
			Date(time.Date(2022, time.February, 28, 12, 34, 56, 789, time.Local)),
			false},
		{
			Date(time.Date(2023, time.February, 28, 12, 34, 56, 789, time.Local)),
			Date(time.Date(2023, time.March, 28, 12, 34, 56, 789, time.Local)),
			false},
		{
			Date(time.Date(2023, time.February, 28, 12, 34, 56, 789, time.Local)),
			Date(time.Date(2023, time.February, 27, 12, 34, 56, 789, time.Local)),
			false},
	}

	for _, v := range tests {
		if b := v.date.Equals(v.other); b != v.expected {
			t.Errorf("Date.Equals returned incorrect value - expected:%v, got:%v", v.expected, b)
		}
	}
}

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
	date := MustParseDate(today.String())

	if date.Before(today) {
		t.Errorf("date '%v' should not be before today (%v)", date, today)
	}

	if today.Before(date) {
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
	date := MustParseDate(today.String())

	if date.After(today) {
		t.Errorf("date '%v' should not be after today (%v)", date, today)
	}

	if today.After(date) {
		t.Errorf("today (%v) should not be after date '%v'", today, date)
	}
}

func TestDateMarshalJSON(t *testing.T) {
	tests := []struct {
		date     Date
		expected string
	}{
		{Date(time.Date(2021, time.February, 28, 12, 34, 56, 345, time.Local)), `"2021-02-28"`},
		{Date{}, `""`},
	}

	for _, v := range tests {
		s, err := json.Marshal(v.date)
		if err != nil {
			t.Fatalf("Unexpected error marshaling Date (%v)", err)
		}

		if string(s) != v.expected {
			t.Errorf("Date incorrectly marshaled - expected:%v, got:%v", v.expected, string(s))
		}
	}
}

func TestDateUnmarshalJSON(t *testing.T) {
	expected := Date(time.Date(2021, time.February, 28, 0, 0, 0, 0, time.Local))
	date := Date{}

	if err := json.Unmarshal([]byte(`"2021-02-28"`), &date); err != nil {
		t.Fatalf("Unexpected error unmarshaling Date (%v)", err)
	}

	if !reflect.DeepEqual(date, expected) {
		t.Errorf("Date incorrectly unmarshaled - expected:%v, got:%v", time.Time(expected), time.Time(date))
	}
}

func TestDateUnmarshalJSONFromEmptyString(t *testing.T) {
	expected := Date{}
	date := Date{}

	if err := json.Unmarshal([]byte(`""`), &date); err != nil {
		t.Fatalf("Unexpected error unmarshaling Date (%v)", err)
	}

	if !reflect.DeepEqual(date, expected) {
		t.Errorf("Date incorrectly unmarshaled - expected:%v, got:%v", time.Time(expected), time.Time(date))
	}
}

func TestDateMarshalUT0311L0xWithZeroValue(t *testing.T) {
	expected := []byte{0x00, 0x00, 0x00, 0x00}
	date := Date{}

	bytes, err := date.MarshalUT0311L0x()
	if err != nil {
		t.Fatalf("Unexpected error marshaling Date (%v)", err)
	}

	if !reflect.DeepEqual(bytes, expected) {
		t.Errorf("Zero value Date incorrectly marshaled - expected:%v, got:%v", expected, bytes)
	}
}

func TestDateUnmarshalUT0311L0x(t *testing.T) {
	tests := []struct {
		bytes    []byte
		expected Date
		isZero   bool
	}{
		{[]byte{0x20, 0x21, 0x02, 0x28}, ToDate(2021, time.February, 28), false},
		{[]byte{0x00, 0x00, 0x00, 0x00}, Date{}, true},
		{[]byte{0x00, 0x01, 0x01, 0x01}, Date{}, true},
	}

	for _, v := range tests {
		var date Date

		if d, err := date.UnmarshalUT0311L0x(v.bytes); err != nil {
			t.Errorf("Error unmarshalling %v (%v)", v.bytes, err)
		} else if d.(*Date).IsZero() != v.isZero {
			t.Errorf("Unmarshalled %v incorrect 'IsZero' - expected:%v, got:%v", v.bytes, v.isZero, d.(*Date).IsZero())
		} else {
			p := fmt.Sprintf("%v", d)
			q := fmt.Sprintf("%v", v.expected)

			if p != q {
				t.Errorf("Invalid date - expected:%v, got:%v", q, p)
			}
		}
	}
}

func TestDateUnmarshalUT0311L0xWithInvalidDate(t *testing.T) {
	var bytes = []byte{0x20, 0x23, 0x13, 0x01}
	var zero = Date{}
	var date Date

	if _, err := date.UnmarshalUT0311L0x(bytes); err != nil {
		t.Errorf("Unexpected error unmarshalling invalid date, got %v", err)
	} else if !date.IsZero() {
		t.Errorf("Expected 'zero' date, got %v", date)
	} else if date != zero {
		t.Errorf("Expected 'zero' date, got %v", date)
	}
}

func TestDatePtrUnmarshalUT0311L0x(t *testing.T) {
	d20210228 := ToDate(2021, time.February, 28)

	tests := []struct {
		bytes    []byte
		expected *Date
		isZero   bool
	}{
		{[]byte{0x20, 0x21, 0x02, 0x28}, &d20210228, false},
		{[]byte{0x00, 0x00, 0x00, 0x00}, nil, true},
		{[]byte{0x00, 0x01, 0x01, 0x01}, nil, true},
	}

	for _, v := range tests {
		var date *Date

		if d, err := date.UnmarshalUT0311L0x(v.bytes); err != nil {
			t.Errorf("Error unmarshalling %v (%v)", v.bytes, err)
		} else if v.expected == nil && d != nil {
			t.Errorf("Unmarshalled %v incorrect date - expected:%v, got:%v", v.bytes, v.expected, d)
		} else if v.expected != nil && d.(*Date).IsZero() != v.isZero {
			t.Errorf("Unmarshalled %v incorrect 'IsZero' - expected:%v, got:%v", v.bytes, v.isZero, d.(*Date).IsZero())
		} else {
			p := fmt.Sprintf("%v", d)
			q := fmt.Sprintf("%v", v.expected)

			if p != q {
				t.Errorf("Invalid date - expected:%v, got:%v", q, p)
			}
		}
	}
}
