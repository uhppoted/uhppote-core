package types

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/uhppoted/uhppote-core/encoding/bcd"
)

type HHmm time.Time

func NewHHmm(hours, minutes int) HHmm {
	return HHmm(time.Date(0, time.January, 1, hours, minutes, 0, 0, time.Local))
}

func HHmmFromString(s string) (*HHmm, error) {
	hhmmss, err := time.ParseInLocation("15:04", s, time.Local)
	if err != nil {
		return nil, err
	}

	t := HHmm(hhmmss)

	return &t, nil
}

func (h HHmm) String() string {
	return time.Time(h).Format("15:04")
}

func (h HHmm) Before(t time.Time) bool {
	p := time.Time(h)

	if p.Hour() < t.Hour() {
		return true
	}

	if p.Hour() == t.Hour() {
		if p.Minute() < t.Minute() {
			return true
		}
	}

	return false
}

func (h HHmm) After(t time.Time) bool {
	p := time.Time(h)

	if p.Hour() > t.Hour() {
		return true
	}

	if p.Hour() == t.Hour() {
		if p.Minute() > t.Minute() {
			return true
		}
	}

	return false
}

func (h HHmm) MarshalUT0311L0x() ([]byte, error) {
	encoded, err := bcd.Encode(time.Time(h).Format("1504"))

	if err != nil {
		return []byte{}, fmt.Errorf("Error encoding HHmm time %v to BCD: [%v]", h, err)
	}

	if encoded == nil {
		return []byte{}, fmt.Errorf("Unknown error encoding HHmm time %v to BCD", h)
	}

	return *encoded, nil
}

func (h *HHmm) UnmarshalUT0311L0x(bytes []byte) (interface{}, error) {
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

func (h HHmm) MarshalJSON() ([]byte, error) {
	return json.Marshal(time.Time(h).Format("15:04"))
}

func (h *HHmm) UnmarshalJSON(bytes []byte) error {
	var s string

	err := json.Unmarshal(bytes, &s)
	if err != nil {
		return err
	}

	t, err := time.ParseInLocation("15:04", s, time.Local)
	if err != nil {
		return err
	}

	*h = HHmm(t)

	return nil
}
