package uhppote

import (
	"testing"

	codec "github.com/uhppoted/uhppote-core/encoding/UTO311-L0x"
)

func TestRecordSpecialEvents(t *testing.T) {
	message := []byte{
		0x17, 0x8e, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := true

	u := mock{
		execute: func(deviceID uint32, request, response interface{}) error {
			return codec.Unmarshal(message, response)
		},
	}

	enabled, err := recordSpecialEvents(&u, 423187757, true)
	if err != nil {
		t.Fatalf("Unexpected error returned from RecordSpecialEvents (%v)", err)
	}

	if enabled != expected {
		t.Errorf("Invalid response:\nexpected:%#v\ngot:     %#v", expected, enabled)
	}
}
