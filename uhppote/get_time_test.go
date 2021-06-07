package uhppote

import (
	"testing"
)

func TestGetTimeWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.GetTime(0)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
