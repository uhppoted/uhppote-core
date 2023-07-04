package uhppote

import (
	"testing"
)

func TestActivateKeypadsWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}
	keypads := map[uint8]bool{
		1: true,
		4: true,
	}

	_, err := u.ActivateKeypads(0, keypads)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
