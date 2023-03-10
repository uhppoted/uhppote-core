package types

import (
	"bytes"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/encoding/bcd"
)

type SystemDate time.Time

func (d SystemDate) IsZero() bool {
	return time.Time(d).IsZero()
}

func (d SystemDate) Format(format string) string {
	return time.Time(d).Format(format)
}

func (d SystemDate) String() string {
	return time.Time(d).Format("2006-01-02")
}

func (d SystemDate) MarshalUT0311L0x() ([]byte, error) {
	encoded, err := bcd.Encode(time.Time(d).Format("060102"))

	if err != nil {
		return []byte{}, fmt.Errorf("error encoding system date %v to BCD: [%v]", d, err)
	}

	if encoded == nil {
		return []byte{}, fmt.Errorf("unknown error encoding system date %v to BCD", d)
	}

	return *encoded, nil
}

func (d *SystemDate) UnmarshalUT0311L0x(b []byte) (interface{}, error) {
	if bytes.Equal(b[0:3], []byte{0, 0, 0}) {
		return &SystemDate{}, nil
	}

	decoded, err := bcd.Decode(b[0:3])
	if err != nil {
		return nil, err
	}

	date, err := time.ParseInLocation("060102", decoded, time.Local)
	if err != nil {
		return nil, err
	}

	v := SystemDate(date)

	return &v, nil
}
