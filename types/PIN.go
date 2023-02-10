package types

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"regexp"
	"strconv"
)

type PIN uint32

func (p PIN) MarshalUT0311L0x() ([]byte, error) {
	bytes := make([]byte, 4)
	binary.LittleEndian.PutUint32(bytes, uint32(p))

	return bytes[:3], nil
}

func (p *PIN) UnmarshalUT0311L0x(bytes []byte) (any, error) {
	b := make([]byte, 4)

	copy(b[0:], bytes[0:3])

	v := binary.LittleEndian.Uint32(b)
	*p = PIN(v)

	return p, nil
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
