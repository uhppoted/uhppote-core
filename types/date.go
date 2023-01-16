package types

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/encoding/bcd"
)

type Date time.Time

func ToDate(year int, month time.Month, day int) Date {
	date := time.Date(year, month, day, 0, 0, 0, 0, time.Local)

	return Date(date)
}

func DateFromString(s string) (Date, error) {
	if date, err := time.ParseInLocation("2006-01-02", s, time.Local); err != nil {
		return Date{}, err
	} else {
		return Date(date), nil
	}
}

func (d Date) IsValid() bool {
	return !time.Time(d).IsZero()
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
	if time.Time(d).IsZero() {
		return ""
	}

	return time.Time(d).Format("2006-01-02")
}

func (d Date) MarshalUT0311L0x() ([]byte, error) {
	if time.Time(d).IsZero() {
		return []byte{0x00, 0x01, 0x01, 0x01}, nil
	}

	encoded, err := bcd.Encode(time.Time(d).Format("20060102"))
	if err != nil {
		return []byte{}, fmt.Errorf("error encoding date %v to BCD: [%v]", d, err)
	} else if encoded == nil {
		return []byte{}, fmt.Errorf("unknown error encoding date %v to BCD", d)
	}

	return *encoded, nil
}

func (d *Date) UnmarshalUT0311L0x(bytes []byte) (interface{}, error) {
	decoded, err := bcd.Decode(bytes[0:4])
	if err != nil {
		return nil, err
	}

	if decoded == "00010101" {
		date := Date{}

		return &date, nil
	}

	date, err := time.ParseInLocation("20060102", decoded, time.Local)
	if err != nil {
		return nil, err
	}

	v := Date(date)

	return &v, nil
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
