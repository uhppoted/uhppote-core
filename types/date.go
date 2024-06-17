package types

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/encoding/bcd"
)

type Date time.Time

// MustParseDate invokes ParseDate and panics on error.
//
// It is intended for use in tests with hard-coded strings.
func MustParseDate(s string) Date {
	if date, err := ParseDate(s); err != nil {
		panic(err)
	} else {
		return date
	}
}

// Parses a date string, returning a zero value Date{} and an
// error if the string is blank or not a valid date.
func ParseDate(s string) (Date, error) {
	if s == "" {
		return Date{}, fmt.Errorf("blank date string")
	} else if date, err := time.ParseInLocation("2006-01-02", s, time.Local); err != nil {
		return Date{}, err
	} else {
		return Date(date), nil
	}
}

// Utility function to explicitly construct a Date from year, month and day.
func ToDate(year int, month time.Month, day int) Date {
	date := time.Date(year, month, day, 0, 0, 0, 0, time.Local)

	return Date(date)
}

// Returns true if the date is the zero value.
func (d Date) IsZero() bool {
	return time.Time(d).IsZero()
}

// Returns true if the year,month and day are the same.
func (d Date) Equals(date Date) bool {
	p := time.Time(d)
	q := time.Time(date)

	return p.Year() == q.Year() && p.Month() == q.Month() && p.Day() == q.Day()
}

func (d Date) Before(date Date) bool {
	p := time.Time(d)
	q := time.Time(date)

	if p.Year() < q.Year() {
		return true
	}

	if p.Year() == q.Year() {
		if p.Month() < q.Month() {
			return true
		}

		if p.Month() == q.Month() {
			if p.Day() < q.Day() {
				return true
			}
		}
	}

	return false
}

func (d Date) After(date Date) bool {
	p := time.Time(d)
	q := time.Time(date)

	if p.Year() > q.Year() {
		return true
	}

	if p.Year() == q.Year() {
		if p.Month() > q.Month() {
			return true
		}

		if p.Month() == q.Month() {
			if p.Day() > q.Day() {
				return true
			}
		}
	}

	return false
}

func (d Date) Weekday() time.Weekday {
	return time.Time(d).Weekday()
}

func (d Date) String() string {
	if d.IsZero() {
		return ""
	} else {
		return time.Time(d).Format("2006-01-02")
	}
}

func (d Date) MarshalUT0311L0x() ([]byte, error) {
	if time.Time(d).IsZero() {
		return []byte{0x00, 0x00, 0x00, 0x00}, nil
	}

	encoded, err := bcd.Encode(time.Time(d).Format("20060102"))
	if err != nil {
		return []byte{}, fmt.Errorf("error encoding date %v to BCD: [%v]", d, err)
	} else if encoded == nil {
		return []byte{}, fmt.Errorf("unknown error encoding date %v to BCD", d)
	}

	return *encoded, nil
}

/*
 * Unmarshalls invalid date values as a 'zero' date without an error on the grounds
 * that it should be possible to retrieve information from a corrupted access controller.
 * Applications are expected to check for valid dates.
 */
func (d *Date) UnmarshalUT0311L0x(bytes []byte) (any, error) {
	decoded, err := bcd.Decode(bytes[0:4])
	if err != nil {
		return nil, err
	}

	if decoded == "00000000" || decoded == "00010101" {
		if d == nil {
			return nil, nil
		} else {
			return &Date{}, nil
		}
	}

	if date, err := time.ParseInLocation("20060102", decoded, time.Local); err != nil {
		return &Date{}, nil
	} else {
		v := Date(date)

		return &v, nil
	}
}

func (d Date) MarshalJSON() ([]byte, error) {
	if time.Time(d).IsZero() {
		return json.Marshal("")
	} else {
		return json.Marshal(time.Time(d).Format("2006-01-02"))
	}
}

func (d *Date) UnmarshalJSON(bytes []byte) error {
	var s string

	err := json.Unmarshal(bytes, &s)
	if err != nil {
		return err
	}

	if s == "" {
		*d = Date(time.Time{})
		return nil
	}

	date, err := time.ParseInLocation("2006-01-02", s, time.Local)
	if err != nil {
		return err
	}

	*d = Date(date)

	return nil
}
