package uhppote

import (
	"testing"
	"time"
)

func TestSetTimeWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.SetTime(0, time.Now())
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
