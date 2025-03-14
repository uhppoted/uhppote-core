package types

import (
	"bytes"
	"encoding/json"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/encoding/bcd"
)

type DateTime time.Time

func DateTimeNow() DateTime {
	return DateTime(time.Now().Truncate(1 * time.Second))
}

// MustParseDateTime invokes ParseDateTime and panics on error.
//
// It is intended for use in tests with hard-coded strings.
func MustParseDateTime(s string) DateTime {
	if datetime, err := ParseDateTime(s); err != nil {
		panic(err)
	} else {
		return datetime
	}
}

// Parses a datetime string, returning a zero value DateTime{} and an
// error if the string is blank or not a valid date/time.
func ParseDateTime(s string) (DateTime, error) {
	if s == "" {
		return DateTime{}, fmt.Errorf("blank date/time string")
	} else if datetime, err := time.ParseInLocation("2006-01-02 15:04:05", s, time.Local); err != nil {
		return DateTime{}, err
	} else {
		return DateTime(datetime), nil
	}
}

func (d DateTime) IsZero() bool {
	return time.Time(d).IsZero()
}

// Because time.Truncate does not in any way behave like your would expect it to :-(
func (d DateTime) Before(t time.Time) bool {
	p := time.Time(d).UnixMilli() / 1000
	q := t.UnixMilli() / 1000

	return p < q
}

func (d DateTime) Add(dt time.Duration) DateTime {
	return DateTime(time.Time(d).Add(dt).Truncate(1 * time.Second))
}

func (d DateTime) String() string {
	if d.IsZero() {
		return ""
	}

	return time.Time(d).Format("2006-01-02 15:04:05")
}

func (d DateTime) MarshalJSON() ([]byte, error) {
	if d.IsZero() {
		return json.Marshal("")
	}

	return json.Marshal(time.Time(d).Format("2006-01-02 15:04:05 MST"))
}

func (d *DateTime) UnmarshalJSON(bytes []byte) error {
	var s string

	err := json.Unmarshal(bytes, &s)
	if err != nil {
		return err
	}

	if s == "" {
		*d = DateTime{}
		return nil
	}

	datetime, err := time.ParseInLocation("2006-01-02 15:04:05", s, time.Local)
	if err != nil {
		datetime, err = time.ParseInLocation("2006-01-02 15:04:05 MST", s, time.Local)
		if err != nil {
			return err
		}
	}

	*d = DateTime(datetime.Truncate(1 * time.Second))

	return nil
}

func (d DateTime) MarshalUT0311L0x() ([]byte, error) {
	encoded, err := bcd.Encode(time.Time(d).Format("20060102150405"))

	if err != nil {
		return []byte{}, fmt.Errorf("error encoding datetime %v to BCD: [%v]", d, err)
	}

	if encoded == nil {
		return []byte{}, fmt.Errorf("unknown error encoding datetime %v to BCD", d)
	}

	return *encoded, nil
}

/*
 * Unmarshalls invalid date/time values as a 'zero' date without an error on the grounds
 * that it should be possible to retrieve information from a corrupted access controller.
 * Applications are expected to check for valid date-times.
 */
func (d *DateTime) UnmarshalUT0311L0x(b []byte) (any, error) {
	if bytes.Equal(b[0:7], []byte{0, 0, 0, 0, 0, 0, 0}) {
		if d == nil {
			return nil, nil
		} else {
			return &DateTime{}, nil
		}
	}

	// ... some uninitialised controllers default to 2000-00-00 00:00:00
	if bytes.Equal(b[0:7], []byte{0x20, 0, 0, 0, 0, 0, 0}) {
		if d == nil {
			return nil, nil
		} else {
			return &DateTime{}, nil
		}
	}

	decoded, err := bcd.Decode(b[0:7])
	if err != nil {
		return nil, err
	}

	datetime, err := time.ParseInLocation("20060102150405", decoded, time.Local)
	if err != nil {
		return &DateTime{}, nil
	}

	v := DateTime(datetime)

	return &v, nil
}

func (d DateTime) MarshalText() ([]byte, error) {
	return []byte(""), nil
}
