package uhppote

import (
	"testing"
)

func TestDeleteCardWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.DeleteCard(0, 8165535)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
