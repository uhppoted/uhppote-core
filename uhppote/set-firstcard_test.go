package uhppote

import (
	"net"
	"testing"
	"time"

	"github.com/uhppoted/uhppote-core/types"
)

func TestSetFirstCard(t *testing.T) {
	message := []byte{
		0x17, 0xaa, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	firstcard := types.FirstCard{
		Door:     3,
		From:     types.MustParseHHmm("08:30"),
		To:       types.MustParseHHmm("16:45"),
		Active:   types.NormallyOpen,
		Inactive: types.NormallyClosed,
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
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	ok, err := u.SetFirstCard(405419896, firstcard)
	if err != nil {
		t.Fatalf("Unexpected error returned from SetFirstCard (%v)", err)
	}

	if !ok {
		t.Errorf("Invalid response:\nexpected:%+v\ngot:     %+v", true, ok)
	}
}

func TestSetFirstCardWithInvalidResponse(t *testing.T) {
	message := []byte{
		0x17, 0x5c, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	}

	firstcard := types.FirstCard{
		Door:     3,
		From:     types.MustParseHHmm("08:30"),
		To:       types.MustParseHHmm("16:45"),
		Active:   types.NormallyOpen,
		Inactive: types.NormallyClosed,
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
			broadcastTo: func(addr *net.UDPAddr, request []byte, handler func([]byte) bool) ([]byte, error) {
				return message, nil
			},
		},
	}

	_, err := u.SetFirstCard(405419896, firstcard)
	if err == nil {
		t.Fatalf("Expected error from SetFirstCard (%v)", err)
	}
}

func TestSetFirstCardWithInvalidDeviceID(t *testing.T) {
	firstcard := types.FirstCard{
		Door:     3,
		From:     types.MustParseHHmm("08:30"),
		To:       types.MustParseHHmm("16:45"),
		Active:   types.NormallyOpen,
		Inactive: types.NormallyClosed,
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
