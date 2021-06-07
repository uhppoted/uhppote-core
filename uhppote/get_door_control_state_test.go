package uhppote

import (
	"testing"
)

func TestGetDoorControlStateWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.GetDoorControlState(0, 3)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
