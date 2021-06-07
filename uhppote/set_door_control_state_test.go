package uhppote

import (
	"testing"
)

func TestSetDoorControlStateInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.SetDoorControlState(0, 3, 1, 5)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
