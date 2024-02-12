package uhppote

import (
	"testing"
)

func TestRestoreDefaultParameterssWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.RestoreDefaultParameters(0)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
