package types

import (
	"encoding/json"
	"fmt"
	"regexp"
	"strconv"
	"time"

	"github.com/uhppoted/uhppote-core/encoding/bcd"
)

type HHmm struct {
	hours   int
	minutes int
}

func NewHHmm(hours, minutes int) HHmm {
	return HHmm{
		hours:   hours,
		minutes: minutes,
	}
}

func HHmmFromTime(t time.Time) HHmm {
	return HHmm{
		hours:   t.Hour(),
		minutes: t.Minute(),
	}
}

func HHmmFromString(s string) (*HHmm, error) {
	re := regexp.MustCompile("^([0-9]{2}):([0-9]{2})$")

	match := re.FindStringSubmatch(s)
	if match == nil || len(match) != 3 {
		return nil, fmt.Errorf("invalid HH:mm string (%s)", s)
	}

	hours, err := strconv.Atoi(match[1])
	if err != nil {
		return nil, fmt.Errorf("invalid HH:mm string (%w)", err)
	}

	minutes, err := strconv.Atoi(match[2])
	if err != nil {
		return nil, fmt.Errorf("invalid HH:mm string (%w)", err)
	}

	if hours < 0 || hours > 24 {
		return nil, fmt.Errorf("invalid HH:mm string (%s) - valid range is 00:00 to 24:00", s)
	}

	if minutes < 0 || minutes > 60 {
		return nil, fmt.Errorf("invalid HH:mm string (%s) - valid range is 00:00 to 24:00", s)
	}

	if hours == 24 && minutes != 0 {
		return nil, fmt.Errorf("invalid HH:mm string (%s) - valid range is 00:00 to 24:00", s)
	}

	return &HHmm{hours: hours, minutes: minutes}, nil
}

func (h HHmm) String() string {
	return fmt.Sprintf("%02d:%02d", h.hours, h.minutes)
}

func (h HHmm) Equals(t HHmm) bool {
	return h.hours == t.hours && h.minutes == t.minutes
}

func (h HHmm) Before(t HHmm) bool {
	return h.before(t)
}

func (h HHmm) before(v interface{}) bool {
	switch t := v.(type) {
	case time.Time:
		if h.hours < t.Hour() {
			return true
		}

		if h.hours == t.Hour() {
			if h.minutes < t.Minute() {
				return true
			}
		}

	case HHmm:
		if h.hours < t.hours {
			return true
		}

		if h.hours == t.hours {
			if h.minutes < t.minutes {
				return true
			}
		}

	default:
		panic("Cannot compare HHmm to anything except time.Time and types.HHmm")
	}

	return false
}

func (h HHmm) After(t HHmm) bool {
	return h.after(t)
}

func (h HHmm) after(v interface{}) bool {
	switch t := v.(type) {
	case time.Time:
		if h.hours > t.Hour() {
			return true
		}

		if h.hours == t.Hour() {
			if h.minutes > t.Minute() {
				return true
			}
		}

	case HHmm:
		if h.hours > t.hours {
			return true
		}

		if h.hours == t.hours {
			if h.minutes > t.minutes {
				return true
			}
		}

	default:
		panic("Cannot compare HHmm to anything except time.Time and types.HHmm")
	}

	return false
}

func (h HHmm) MarshalUT0311L0x() ([]byte, error) {
	encoded, err := bcd.Encode(fmt.Sprintf("%02d%02d", h.hours, h.minutes))

	if err != nil {
		return []byte{}, fmt.Errorf("error encoding HHmm time %v to BCD: [%v]", h, err)
	}

	if encoded == nil {
		return []byte{}, fmt.Errorf("unknown error encoding HHmm time %v to BCD", h)
	}

	return *encoded, nil
}

func (h *HHmm) UnmarshalUT0311L0x(bytes []byte) (interface{}, error) {
	decoded, err := bcd.Decode(bytes[0:2])
	if err != nil {
		return nil, err
	}

	re := regexp.MustCompile("^([0-9]{2})([0-9]{2})$")

	match := re.FindStringSubmatch(decoded)
	if match == nil || len(match) != 3 {
		return nil, fmt.Errorf("invalid HH:mm string (%s)", decoded)
	}

	hours, err := strconv.Atoi(match[1])
	if err != nil {
		return nil, fmt.Errorf("invalid HH:mm string (%w)", err)
	}

	minutes, err := strconv.Atoi(match[2])
	if err != nil {
		return nil, fmt.Errorf("invalid HH:mm string (%w)", err)
	}

	if hours < 0 || hours > 24 {
		return nil, fmt.Errorf("invalid HH:mm string (%s) - valid range is 00:00 to 24:00", decoded)
	}

	if minutes < 0 || minutes > 60 {
		return nil, fmt.Errorf("invalid HH:mm string (%s) - valid range is 00:00 to 24:00", decoded)
	}

	if hours == 24 && minutes != 0 {
		return nil, fmt.Errorf("invalid HH:mm string (%s) - valid range is 00:00 to 24:00", decoded)
	}

	return &HHmm{hours: hours, minutes: minutes}, nil
}

func (h HHmm) MarshalJSON() ([]byte, error) {
	return json.Marshal(fmt.Sprintf("%02d:%02d", h.hours, h.minutes))
}

func (h *HHmm) UnmarshalJSON(bytes []byte) error {
	var s string

	err := json.Unmarshal(bytes, &s)
	if err != nil {
		return err
	}

	re := regexp.MustCompile("^([0-9]{2}):([0-9]{2})$")

	match := re.FindStringSubmatch(s)
	if match == nil || len(match) != 3 {
		return fmt.Errorf("invalid HH:mm string (%s)", s)
	}

	hours, err := strconv.Atoi(match[1])
	if err != nil {
		return fmt.Errorf("invalid HH:mm string (%w)", err)
	}

	minutes, err := strconv.Atoi(match[2])
	if err != nil {
		return fmt.Errorf("invalid HH:mm string (%w)", err)
	}

	if hours < 0 || hours > 24 {
		return fmt.Errorf("invalid HH:mm string (%s) - valid range is 00:00 to 24:00", s)
	}

	if minutes < 0 || minutes > 60 {
		return fmt.Errorf("invalid HH:mm string (%s) - valid range is 00:00 to 24:00", s)
	}

	if hours == 24 && minutes != 0 {
		return fmt.Errorf("invalid HH:mm string (%s) - valid range is 00:00 to 24:00", s)
	}

	*h = HHmm{
		hours:   hours,
		minutes: minutes,
	}

	return nil
}
