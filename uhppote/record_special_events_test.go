package uhppote

import (
	"net"
	"testing"
)

func TestRecordSpecialEvents(t *testing.T) {
	message := []byte{
		0x17, 0x8e, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := true

	u := uhppote{
		driver: &stub{
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	enabled, err := u.RecordSpecialEvents(423187757, true)
	if err != nil {
		t.Fatalf("Unexpected error returned from RecordSpecialEvents (%v)", err)
	}

	if enabled != expected {
		t.Errorf("Invalid response:\nexpected:%#v\ngot:     %#v", expected, enabled)
	}
}

func TestRecordSpecialEventsWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.RecordSpecialEvents(0, true)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
