package uhppote

import (
	"testing"
)

func TestSetEventIndexWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.SetEventIndex(0, 17)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
