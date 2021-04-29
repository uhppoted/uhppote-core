package types

import (
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/encoding/bcd"
)

type Time struct {
	SerialNumber SerialNumber
	DateTime     DateTime
}

func (t Time) String() string {
	return fmt.Sprintf("%s %s", t.SerialNumber, t.DateTime.String())
}

type HHmm time.Time

func HHmmFromString(s string) (*HHmm, error) {
	hhmmss, err := time.ParseInLocation("15:04", s, time.Local)
	if err != nil {
		return nil, err
	}

	t := HHmm(hhmmss)

	return &t, nil
}

func (t HHmm) String() string {
	return time.Time(t).Format("15:04")
}

func (t HHmm) MarshalUT0311L0x() ([]byte, error) {
	encoded, err := bcd.Encode(time.Time(t).Format("1504"))

	if err != nil {
		return []byte{}, fmt.Errorf("Error encoding HHmm time %v to BCD: [%v]", t, err)
	}

	if encoded == nil {
		return []byte{}, fmt.Errorf("Unknown error encoding HHmm time %v to BCD", t)
	}

	return *encoded, nil
}

func (t *HHmm) UnmarshalUT0311L0x(bytes []byte) (interface{}, error) {
	decoded, err := bcd.Decode(bytes[0:2])
	if err != nil {
		return nil, err
	}

	time, err := time.ParseInLocation("1504", decoded, time.Local)
	if err != nil {
		return nil, err
	}

	v := HHmm(time)

	return &v, nil
}
