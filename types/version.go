package types

import (
	"encoding/binary"
	"encoding/json"
	"errors"
	"fmt"
)

type Version uint16

func (v Version) String() string {
	major := uint16(v) >> 8
	minor := uint16(v) & 0x00ff

	return fmt.Sprintf("%x.%x", major, minor)
}

func (v Version) MarshalUT0311L0x() ([]byte, error) {
	bytes := make([]byte, 2)
	binary.BigEndian.PutUint16(bytes, uint16(v))

	return bytes, nil
}

func (v *Version) UnmarshalUT0311L0x(bytes []byte) (interface{}, error) {
	vv := Version(binary.BigEndian.Uint16(bytes))

	return &vv, nil
}

func (v Version) MarshalJSON() ([]byte, error) {
	return json.Marshal(fmt.Sprintf("%04x", uint16(v)))
}

func (v *Version) UnmarshalJSON(bytes []byte) error {
	var s string

	err := json.Unmarshal(bytes, &s)
	if err != nil {
		return err
	}

	N, err := fmt.Sscanf(s, "%04x", v)
	if err != nil {
		return err
	}

	if N != 1 {
		return errors.New("Unable to extract 'version' from JSON file")
	}

	return nil
}
