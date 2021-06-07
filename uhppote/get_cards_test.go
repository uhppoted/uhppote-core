package uhppote

import (
	"testing"
)

func TestGetCardsWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.GetCards(0)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
