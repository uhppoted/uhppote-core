package uhppote

import (
	"net"
	"testing"
	"time"

	"github.com/uhppoted/uhppote-core/types"
)

func TestSetFirstCard(t *testing.T) {
	message := []byte{
		0x17, 0xaa, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	firstcard := types.FirstCard{
		Door:      3,
		Start:     hhmm("08:30"),
		End:       hhmm("17:45"),
		StartMode: types.Open,
		EndMode:   types.Closed,
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
			},
		},
	}

	ok, err := u.SetFirstCard(423187757, firstcard)
	if err != nil {
		t.Fatalf("Unexpected error returned from SetFirstCard (%v)", err)
	}

	if !ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", true, ok)
	}
}

func TestSetFirstCardWithInvalidResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5c, 0x00, 0x00, 0x2d, 0x55, 0x39, 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	firstcard := types.FirstCard{
		Door:      3,
		Start:     hhmm("08:30"),
		StartMode: 1,
		End:       hhmm("17:45"),
		EndMode:   2,
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	u := uhppote{
		driver: &stub{
			send: func(request []byte, addr *net.UDPAddr, handler func([]byte) bool) error {
				handler(message)
				return nil
			},
		},
	}

	_, err := u.SetFirstCard(423187757, firstcard)
	if err == nil {
		t.Fatalf("Expected error from SetFirstCard (%v)", err)
	}
}

func TestSetFirstCardWithInvalidDeviceID(t *testing.T) {
	firstcard := types.FirstCard{
		Door:      3,
		Start:     hhmm("08:30"),
		StartMode: types.Open,
		End:       hhmm("17:45"),
		EndMode:   types.Closed,
		Weekdays: types.Weekdays{
			time.Monday:    true,
			time.Tuesday:   true,
			time.Wednesday: false,
			time.Thursday:  true,
			time.Friday:    false,
			time.Saturday:  true,
			time.Sunday:    true,
		},
	}

	u := uhppote{}

	_, err := u.SetFirstCard(0, firstcard)
	if err == nil {
		t.Fatalf("Expected 'Invalid device ID' error, got %v", err)
	}
}
