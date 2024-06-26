package uhppote

import (
	"net"
	"testing"
)

func TestClearTaskList(t *testing.T) {
	message := []byte{
		0x17, 0xa6, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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

	enabled, err := u.ClearTaskList(423187757)
	if err != nil {
		t.Fatalf("Unexpected error returned from ClearTaskList (%v)", err)
	}

	if enabled != expected {
		t.Errorf("Invalid response:\nexpected:%#v\ngot:     %#v", expected, enabled)
	}
}

func TestClearTaskListWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.ClearTaskList(0)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
