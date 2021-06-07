package uhppote

import (
	"testing"
)

func TestGetListenerWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.GetListener(0)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
