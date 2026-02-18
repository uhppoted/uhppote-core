package types

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/encoding/bcd"
)

type SystemTime time.Time

func TimeFromString(s string) (*SystemTime, error) {
	hhmmss, err := time.ParseInLocation("15:04:05", s, time.Local)
	if err != nil {
		return nil, err
	}

	t := SystemTime(hhmmss)
	return &t, nil
}

func (t SystemTime) Format(format string) string {
	return time.Time(t).Format(format)
}

func (t SystemTime) String() string {
	return time.Time(t).Format("15:04:05")
}

func (d SystemTime) MarshalUT0311L0x() ([]byte, error) {
	encoded, err := bcd.Encode(time.Time(d).Format("150405"))

	if err != nil {
		return []byte{}, fmt.Errorf("error encoding system time %v to BCD: [%v]", d, err)
	}

	if encoded == nil {
		return []byte{}, fmt.Errorf("unknown error encoding system time %v to BCD", d)
	}

	return *encoded, nil
}

func (t *SystemTime) UnmarshalUT0311L0x(bytes []byte) (any, error) {
	decoded, err := bcd.Decode(bytes[0:3])
	if err != nil {
		return nil, err
	}

	time, err := time.ParseInLocation("150405", decoded, time.Local)
	if err != nil {
		return nil, err
	}

	v := SystemTime(time)

	return &v, nil
}
