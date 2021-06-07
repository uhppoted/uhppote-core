package uhppote

import (
	"testing"

	"github.com/uhppoted/uhppote-core/types"
)

func TestPutCardWithInvalidDeviceID(t *testing.T) {
	u := uhppote{}

	_, err := u.PutCard(0, types.Card{})
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
