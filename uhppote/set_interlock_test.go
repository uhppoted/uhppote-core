package uhppote

import (
	"testing"

	"github.com/uhppoted/uhppote-core/types"
)

func TestSetInterlockWithInvalidControllerID(t *testing.T) {
	u := uhppote{}

	_, err := u.SetInterlock(0, types.Interlock12_34)
	if err == nil {
		t.Fatalf("Expected 'Invalid controller ID' error, got %v", err)
	}
}
