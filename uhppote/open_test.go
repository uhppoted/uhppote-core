package uhppote

import (
	"testing"
)

func TestOpenDoorWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.OpenDoor(0, 3)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
