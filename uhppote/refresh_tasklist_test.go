package uhppote

import (
	"net"
	"testing"
)

func TestRefreshTaskList(t *testing.T) {
	message := []byte{
		0x17, 0xac, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	expected := true

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
			},
		},
	}

	refreshed, err := u.RefreshTaskList(423187757)
	if err != nil {
		t.Fatalf("Unexpected error returned from RefreshTaskList (%v)", err)
	}

	if refreshed != expected {
		t.Errorf("Invalid response:\nexpected:%#v\ngot:     %#v", expected, refreshed)
	}
}

func TestRefreshTaskListWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.RefreshTaskList(0)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
