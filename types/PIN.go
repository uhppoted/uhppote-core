package types

import (
	"encoding/json"
	"fmt"
	"regexp"
	"strconv"

	"github.com/uhppoted/uhppote-core/encoding/bcd"
)

type PIN uint32

func (p PIN) MarshalUT0311L0x() ([]byte, error) {
	var v string

	if p < 1000000 {
		v = fmt.Sprintf("%06v", p)
	} else {
		v = fmt.Sprintf("%06v", 0)
	}

	if encoded, err := bcd.Encode(v); err != nil {
		return []byte{}, fmt.Errorf("error encoding PIN %v as BCD: [%v]", p, err)
	} else if encoded == nil {
		return []byte{}, fmt.Errorf("unknown error encoding PIN %v as BCD", p)
	} else {
		return *encoded, nil
	}
}

func (p *PIN) UnmarshalUT0311L0x(bytes []byte) (any, error) {
	if s, err := bcd.Decode(bytes[0:3]); err != nil {
		return nil, err
	} else if v, err := strconv.ParseUint(s, 10, 32); err != nil {
		return nil, err
	} else if v > 999999 {
		return nil, fmt.Errorf("invalid PIN (%v)", v)
	} else {
		pin := PIN(v)
		*p = pin

		return &pin, nil
	}
}

func (p PIN) MarshalJSON() ([]byte, error) {
	if p == 0 || p > 999999 {
		return json.Marshal("")
	} else {
		return json.Marshal(fmt.Sprintf("%v", p))
	}
}

func (p *PIN) UnmarshalJSON(bytes []byte) error {
	var s string

	if err := json.Unmarshal(bytes, &s); err != nil {
		return err
	} else if !regexp.MustCompile(`^[0-9]{0,6}$`).MatchString(s) {
		return fmt.Errorf("invalid PIN (%v)", s)
	} else if s == "" {
		*p = PIN(0)
	} else if v, err := strconv.ParseUint(s, 10, 32); err != nil {
		return err
	} else {
		*p = PIN(v)
	}

	return nil
}
