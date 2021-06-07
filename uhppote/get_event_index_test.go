package uhppote

import (
	"testing"
)

func TestGetEventIndexWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.GetEventIndex(0)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
