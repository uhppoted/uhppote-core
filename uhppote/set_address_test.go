package uhppote

import (
	"net"
	"testing"
)

func TestSetAddressWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.SetAddress(0, net.IPv4(192, 168, 1, 100), net.IPv4(255, 255, 255, 0), net.IPv4(192, 168, 1, 255))
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
