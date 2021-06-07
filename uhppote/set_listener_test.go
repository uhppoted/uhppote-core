package uhppote

import (
	"net"
	"testing"
)

func TestSetListenerWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.SetListener(0, net.UDPAddr{
		IP:   net.IPv4(192, 168, 1, 100),
		Port: 60001,
		Zone: "",
	})

	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
