package uhppote

import (
	"testing"
)

func TestGetStatusWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.GetStatus(0)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
